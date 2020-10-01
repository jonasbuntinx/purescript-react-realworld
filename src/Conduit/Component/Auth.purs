module Conduit.Component.Auth where

import Prelude
import Apiary as Apiary
import Conduit.Api.Endpoints (GetUser)
import Conduit.Api.Utils (addBaseUrl, addToken)
import Conduit.Data.Auth (Auth, toAuth)
import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Timer as Timer
import Foreign.Day (now)
import Foreign.Generic (decodeJSON, encodeJSON)
import React.Basic.Hooks as React
import Record as Record
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage
import Wire.React.Atom.Class (modify, read)
import Wire.React.Atom.Sync as Sync

mkAuthManager ::
  Effect
    { signal :: Sync.Sync (Maybe Auth)
    , authManager :: React.JSX
    }
mkAuthManager = do
  authSignal <- create
  component <-
    React.component "AuthManager" \_ -> React.do
      state /\ setState <- React.useState { interval: Nothing }
      React.useEffectOnce do
        checkAuthStatus authSignal
        authCheckInterval <- Timer.setInterval 900_0000 (checkAuthStatus authSignal)
        setState _ { interval = Just authCheckInterval }
        pure $ traverse_ Timer.clearInterval state.interval
      pure React.empty
  pure { signal: authSignal, authManager: component unit }
  where
  refreshToken authSignal = do
    auth <- read authSignal
    for_ auth \{ token } -> do
      launchAff_ do
        res <- Apiary.makeRequest (Apiary.Route :: GetUser) (addBaseUrl <<< addToken token) Apiary.none Apiary.none Apiary.none
        liftEffect case hush $ Variant.match { ok: _.user } <$> res of
          Nothing -> resetAuth authSignal
          Just user -> writeAuth authSignal user

  checkAuthStatus authSignal = do
    auth <- read authSignal
    for_ auth \{ expirationTime } -> do
      now <- now
      if now > expirationTime then
        resetAuth authSignal
      else
        refreshToken authSignal

  writeAuth authSignal user = modify authSignal $ const $ toAuth user.token (Just $ Record.delete (SProxy :: _ "token") user)

  resetAuth authSignal = modify authSignal $ const Nothing

  create =
    Sync.create
      { load:
          do
            localStorage <- Window.localStorage =<< window
            item <- Storage.getItem "token" localStorage
            pure $ flip toAuth Nothing =<< (hush <<< runExcept <<< decodeJSON) =<< item
      , save:
          case _ of
            Nothing -> do
              localStorage <- Window.localStorage =<< window
              Storage.removeItem "token" localStorage
            Just { token } -> do
              localStorage <- Window.localStorage =<< window
              Storage.setItem "token" (encodeJSON token) localStorage
      }
