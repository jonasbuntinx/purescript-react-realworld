module Conduit.Component.Auth where

import Prelude
import Affjax.StatusCode (StatusCode(..))
import Conduit.Api.Client (Error, makeSecureRequest')
import Conduit.Api.Endpoint as Endpoint
import Conduit.Data.Auth (Auth, toAuth)
import Conduit.Data.User (CurrentUser)
import Control.Monad.Except (runExcept)
import Data.Either (Either, hush)
import Data.Foldable (for_, traverse_)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Timer as Timer
import FRP.Event as Event
import Foreign.Day (now)
import Foreign.Generic (decodeJSON, encodeJSON)
import React.Basic.Hooks as React
import Record as Record
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage

mkAuthManager ::
  Effect
    { read :: Effect (Maybe Auth)
    , event :: Event.Event (Maybe Auth)
    , modify :: (Maybe Auth -> Maybe Auth) -> Effect (Maybe Auth)
    , component :: React.JSX
    }
mkAuthManager = do
  { event, read, modify } <- create
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
    { read
    , event
    , modify
    , component: component unit
    }
  where
  create = do
    initial <- load
    value <- Ref.new initial
    { event, push } <- Event.create
    pure
      { event: event
      , read: Ref.read value
      , modify:
          \f -> do
            newValue <- Ref.modify f value
            save newValue
            push newValue
            pure newValue
      }

  load = do
    localStorage <- Window.localStorage =<< window
    item <- Storage.getItem "token" localStorage
    pure $ flip toAuth Nothing =<< (hush <<< runExcept <<< decodeJSON) =<< item

  save = case _ of
    Nothing -> do
      localStorage <- Window.localStorage =<< window
      Storage.removeItem "token" localStorage
    Just { token } -> do
      localStorage <- Window.localStorage =<< window
      Storage.setItem "token" (encodeJSON token) localStorage

  refresh { read, modify } = do
    auth <- read
    for_ auth \{ expirationTime, token } -> do
      now <- now
      if now > expirationTime then
        void $ modify $ const Nothing
      else
        launchAff_ do
          (res :: Either Error { user :: CurrentUser }) <- makeSecureRequest' token GET (StatusCode 200) Endpoint.User unit
          liftEffect case hush $ _.user <$> res of
            Nothing -> void $ modify $ const Nothing
            Just user -> void $ modify $ const $ toAuth user.token (Just $ Record.delete (SProxy :: _ "token") user)
