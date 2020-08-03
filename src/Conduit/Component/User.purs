module Conduit.Component.User where

import Prelude
import Apiary.Client (makeRequest) as Apiary
import Apiary.Route (Route(..)) as Apiary
import Apiary.Types (none) as Apiary
import Conduit.Api.User (GetUser)
import Conduit.Api.Utils (addBaseUrl, addToken)
import Conduit.Data.Route (Route(..))
import Conduit.Effects.Routing (redirect)
import Conduit.Env.User (UserSignal, create, logout', refreshToken', unpack)
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Minutes(..), convertDuration)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Timer as Timer
import Foreign.Moment as Moment
import React.Basic.Hooks as React
import Wire.React.Class (read)

mkUserManager :: Effect (Tuple UserSignal (React.JSX -> React.JSX))
mkUserManager = do
  userSignal <- create
  component <-
    React.component "UserManager" \content -> React.do
      state /\ setState <- React.useState { interval: Nothing }
      React.useEffectOnce do
        refreshToken userSignal
        authCheckInterval <- Timer.setInterval 5_000 (checkAuthStatus userSignal)
        setState _ { interval = Just authCheckInterval }
        pure $ traverse_ Timer.clearInterval state.interval
      pure content
  pure $ Tuple userSignal component
  where
  tokenRefreshInterval = convertDuration $ Minutes 15.0

  onSessionExpire = redirect Home

  refreshToken userSignal = do
    user <- read userSignal
    for_ (fst user) \{ token } -> do
      launchAff_ do
        res <- liftAff $ Apiary.makeRequest (Apiary.Route :: GetUser) (addBaseUrl <<< addToken token) Apiary.none Apiary.none Apiary.none
        liftEffect
          $ case res of
              Left _ -> logout' userSignal *> onSessionExpire
              Right success -> success # Variant.match { ok: refreshToken' userSignal <<< _.token <<< _.user }

  checkAuthStatus userSignal = do
    user <- read userSignal
    for_ (unpack =<< fst user) \{ updated, expirationTime } -> do
      now <- Moment.now
      if now > expirationTime then
        logout' userSignal *> onSessionExpire
      else
        if now > (updated <> tokenRefreshInterval) then do
          refreshToken userSignal
        else
          pure unit
