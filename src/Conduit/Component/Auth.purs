module Conduit.Component.Auth where

import Prelude
import Conduit.Data.Route (Route(..))
import Conduit.Effects.Routing (redirect)
import Conduit.State.Auth (AuthState, create, login', logout')
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..))
import Data.Moment as Moment
import Data.Time.Duration (Hours(..), Minutes(..), convertDuration)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Timer as Timer
import React.Basic.Hooks as React
import Wire.React.Class (read)

mkAuthManager ::
  Effect (Tuple AuthState (React.JSX -> React.JSX))
mkAuthManager = do
  authState <- create
  component <-
    React.component "AuthManager" \content -> React.do
      state /\ setState <- React.useState { interval: Nothing }
      React.useEffectOnce do
        refreshToken authState
        authCheckInterval <- Timer.setInterval 5_000 (checkAuthStatus authState)
        setState _ { interval = Just authCheckInterval }
        pure $ traverse_ Timer.clearInterval state.interval
      pure content
  pure $ Tuple authState component
  where
  tokenRefreshInterval = convertDuration $ Minutes 15.0

  tokenLifetime = convertDuration $ Hours 1.0

  onSessionExpire = redirect Home

  refreshToken authState = do
    auth <- read authState
    for_ auth \{ token } -> do
      -- | Call the refresh token endpoint
      login' authState token

  checkAuthStatus authState = do
    auth <- read authState
    for_ auth \{ token, updated } -> do
      now <- Moment.now
      if now > (updated <> tokenLifetime) then
        logout' authState *> onSessionExpire
      else
        if now > (updated <> tokenRefreshInterval) then do
          refreshToken authState
        else
          pure unit
