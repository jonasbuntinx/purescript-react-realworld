module Conduit.Component.Auth where

import Prelude
import Apiary.Client (makeRequest) as Apiary
import Apiary.Route (Route(..)) as Apiary
import Apiary.Types (none) as Apiary
import Conduit.Api.Endpoints (GetUser)
import Conduit.Api.Utils (addBaseUrl, addToken)
import Conduit.Data.Route (Route(..))
import Conduit.Env.Auth (AuthSignal, create, logout', refreshToken')
import Conduit.Env.Routing (redirect)
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Timer as Timer
import Foreign.Day (now)
import React.Basic.Hooks as React
import Wire.React.Class (read)

mkAuthManager :: Effect (AuthSignal /\ (React.JSX -> React.JSX))
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
  pure $ authSignal /\ component
  where
  onSessionExpire = redirect Home

  refreshToken authSignal = do
    auth <- read authSignal
    for_ auth \{ token } -> do
      launchAff_ do
        res <- liftAff $ Apiary.makeRequest (Apiary.Route :: GetUser) (addBaseUrl <<< addToken token) Apiary.none Apiary.none Apiary.none
        liftEffect
          $ case res of
              Left _ -> logout' authSignal *> onSessionExpire
              Right success -> success # Variant.match { ok: refreshToken' authSignal <<< _.token <<< _.user }

  checkAuthStatus authSignal = do
    auth <- read authSignal
    for_ auth \{ expirationTime } -> do
      now <- now
      if now > expirationTime then
        logout' authSignal *> onSessionExpire
      else
        refreshToken authSignal
