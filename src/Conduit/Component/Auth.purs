module Conduit.Component.Auth where

import Prelude
import Apiary.Client (makeRequest) as Apiary
import Apiary.Route (Route(..)) as Apiary
import Apiary.Types (none) as Apiary
import Conduit.Api.Endpoints (GetUser)
import Conduit.Api.Utils (addBaseUrl, addToken)
import Conduit.Data.Jwt as Jwt
import Conduit.Env.Auth (AuthEnv, create)
import Data.Either (Either(..), hush)
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Timer as Timer
import Foreign.Day (fromMilliseconds, now)
import React.Basic.Hooks as React
import Wire.React.Class (modify, read)

mkAuthManager :: Effect (AuthEnv /\ (React.JSX -> React.JSX))
mkAuthManager = do
  authSignal <- create
  component <-
    React.component "AuthManager" \content -> React.do
      state /\ setState <- React.useState { interval: Nothing }
      React.useEffectOnce do
        refreshToken authSignal
        authCheckInterval <- Timer.setInterval 900_0000 (checkAuthStatus authSignal)
        setState _ { interval = Just authCheckInterval }
        pure $ traverse_ Timer.clearInterval state.interval
      pure content
  pure
    $ { signal: authSignal
      , read: read authSignal
      , login: login authSignal
      , logout: logout authSignal
      , updateProfile: updateProfile authSignal
      }
    /\ component
  where
  refreshToken authSignal = do
    auth <- read authSignal
    for_ auth \{ token } -> do
      launchAff_ do
        res <- liftAff $ Apiary.makeRequest (Apiary.Route :: GetUser) (addBaseUrl <<< addToken token) Apiary.none Apiary.none Apiary.none
        liftEffect
          $ case res of
              Left _ -> logout authSignal
              Right success -> success # Variant.match { ok: updateToken authSignal <<< _.token <<< _.user }

  checkAuthStatus authSignal = do
    auth <- read authSignal
    for_ auth \{ expirationTime } -> do
      now <- now
      if now > expirationTime then
        logout authSignal
      else
        refreshToken authSignal

  updateToken authSignal token = modify authSignal $ map $ _ { token = token }

  login authSignal token profile =
    modify authSignal \_ -> do
      { exp, username } <- hush $ Jwt.decode token
      pure { token, username, expirationTime: fromMilliseconds $ Milliseconds $ exp * 1000.0, profile: Just profile }

  logout authSignal = modify authSignal $ const Nothing

  updateProfile authSignal profile = modify authSignal $ map $ _ { profile = Just profile }
