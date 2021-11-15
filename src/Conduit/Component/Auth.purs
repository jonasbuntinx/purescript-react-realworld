module Conduit.Component.Auth where

import Prelude
import Affjax.StatusCode (StatusCode(..))
import Conduit.Api.Client (Error, makeSecureRequest')
import Conduit.Api.Endpoint as Endpoint
import Conduit.Data.Auth (Auth, toAuth)
import Conduit.Data.User (CurrentUser, currentUserCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either, hush)
import Data.Foldable (for_, traverse_)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Timer as Timer
import Foreign.Day (now)
import Halogen.Subscription as HS
import React.Basic.Hooks as React
import Record as Record
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage

type AuthIO
  =
  { read :: Effect (Maybe Auth)
  , emitter :: HS.Emitter (Maybe Auth)
  , modify :: (Maybe Auth -> Maybe Auth) -> Effect (Maybe Auth)
  }

mkAuthManager :: Effect (AuthIO /\ React.JSX)
mkAuthManager = do
  { read, emitter, modify } <- create
  component <-
    React.component "AuthManager" \_ -> React.do
      state /\ setState <- React.useState { interval: Nothing }
      React.useEffectOnce do
        refresh { read, modify }
        interval <- Timer.setInterval 900_0000 (refresh { read, modify })
        setState _ { interval = Just interval }
        pure $ traverse_ Timer.clearInterval state.interval
      pure React.empty
  pure
    ( { read
      , emitter
      , modify
      }
        /\ component unit
    )
  where
  create = do
    initial <- load
    value <- Ref.new initial
    { emitter, listener } <- HS.create
    pure
      { read: Ref.read value
      , emitter
      , modify:
          \f -> do
            newValue <- Ref.modify f value
            save newValue
            HS.notify listener newValue
            pure newValue
      }

  load = do
    localStorage <- Window.localStorage =<< window
    item <- Storage.getItem "token" localStorage
    pure $ flip toAuth Nothing =<< item

  save = case _ of
    Nothing -> do
      localStorage <- Window.localStorage =<< window
      Storage.removeItem "token" localStorage
    Just { token } -> do
      localStorage <- Window.localStorage =<< window
      Storage.setItem "token" token localStorage

  refresh { read, modify } = do
    auth <- read
    for_ auth \{ expirationTime, token } -> do
      now <- now
      if now > expirationTime then
        void $ modify $ const Nothing
      else
        launchAff_ do
          (res :: Either Error { user :: CurrentUser }) <- makeSecureRequest' token GET (StatusCode 200) Endpoint.User CA.null (CAR.object "UserResponse" { user: currentUserCodec }) unit
          liftEffect case hush $ _.user <$> res of
            Nothing -> void $ modify $ const Nothing
            Just user -> void $ modify $ const $ toAuth user.token (Just $ Record.delete (SProxy :: SProxy "token") user)
