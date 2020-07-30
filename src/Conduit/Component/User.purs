module Conduit.Component.User where

import Prelude
import Apiary.Client (makeRequest) as Apiary
import Apiary.Route (Route(..)) as Apiary
import Apiary.Types (none) as Apiary
import Conduit.Api.User (GetUser)
import Conduit.Api.Utils (addBaseUrl, addToken)
import Conduit.Data.Route (Route(..))
import Conduit.Effects.Routing (redirect)
import Conduit.State.User as UserState
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..))
import Foreign.Moment as Moment
import Data.Time.Duration (Hours(..), Minutes(..), convertDuration)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Timer as Timer
import React.Basic.Hooks as React
import Wire.React.Class (read)

mkUserManager ::
  Effect (Tuple UserState.UserState (React.JSX -> React.JSX))
mkUserManager = do
  userState <- UserState.create
  component <-
    React.component "UserManager" \content -> React.do
      state /\ setState <- React.useState { interval: Nothing }
      React.useEffectOnce do
        refreshToken userState
        authCheckInterval <- Timer.setInterval 5_000 (checkAuthStatus userState)
        setState _ { interval = Just authCheckInterval }
        pure $ traverse_ Timer.clearInterval state.interval
      pure content
  pure $ Tuple userState component
  where
  tokenRefreshInterval = convertDuration $ Minutes 15.0

  tokenLifetime = convertDuration $ Hours 1.0

  onSessionExpire = redirect Home

  refreshToken userState = do
    user <- read userState
    for_ (fst user) \{ token } -> do
      launchAff_ do
        res <- liftAff $ Apiary.makeRequest (Apiary.Route :: GetUser) (addBaseUrl <<< addToken token) Apiary.none Apiary.none Apiary.none
        liftEffect
          $ case res of
              Left _ -> UserState.logout' userState *> onSessionExpire
              Right success -> success # Variant.match { ok: UserState.refreshToken' userState <<< _.token <<< _.user }

  checkAuthStatus userState = do
    user <- read userState
    for_ (fst user) \{ token, updated } -> do
      now <- Moment.now
      if now > (updated <> tokenLifetime) then
        UserState.logout' userState *> onSessionExpire
      else
        if now > (updated <> tokenRefreshInterval) then do
          refreshToken userState
        else
          pure unit
