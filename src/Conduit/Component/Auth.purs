module Conduit.Component.Auth where

import Prelude
import Apiary.Client (makeRequest) as Apiary
import Apiary.Route (Route(..)) as Apiary
import Apiary.Types (none) as Apiary
import Conduit.Api.Endpoints (GetUser)
import Conduit.Api.Utils (addBaseUrl, addToken)
import Conduit.Data.Auth (Auth)
import Conduit.Data.Jwt as Jwt
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), hush)
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Timer as Timer
import Foreign.Day (fromMilliseconds, now)
import Foreign.Generic (decodeJSON, encodeJSON)
import React.Basic.Hooks as React
import Record as Record
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage
import Wire.React.Atom.Async as Async
import Wire.React.Atom.Class (modify, read)
import Wire.React.Atom.Selector as Selector
import Wire.React.Atom.Sync as Sync

type AuthEnv
  = { signal :: Selector.Selector (Maybe Auth)
    }

mkAuthManager ::
  Effect (AuthEnv /\ (React.JSX -> React.JSX))
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
  pure $ { signal: authSignal } /\ component
  where
  refreshToken authSignal = do
    auth <- read authSignal
    for_ auth \{ token } -> do
      launchAff_ do
        res <- liftAff $ Apiary.makeRequest (Apiary.Route :: GetUser) (addBaseUrl <<< addToken token) Apiary.none Apiary.none Apiary.none
        liftEffect
          $ case res of
              Left _ -> reset authSignal
              Right success -> success # Variant.match { ok: updateToken authSignal <<< _.token <<< _.user }

  checkAuthStatus authSignal = do
    auth <- read authSignal
    for_ auth \{ expirationTime } -> do
      now <- now
      if now > expirationTime then
        reset authSignal
      else
        refreshToken authSignal

  updateToken authSignal token = modify authSignal $ map $ _ { token = token }

  reset authSignal = modify authSignal $ const Nothing

  create = do
    tokenSignal <-
      Sync.create
        { load:
            do
              localStorage <- Window.localStorage =<< window
              item <- Storage.getItem "token" localStorage
              pure $ (hush <<< runExcept <<< decodeJSON) =<< item
        , save:
            case _ of
              Nothing -> do
                localStorage <- Window.localStorage =<< window
                Storage.removeItem "token" localStorage
              Just token -> do
                localStorage <- Window.localStorage =<< window
                Storage.setItem "token" (encodeJSON token) localStorage
        }
    profileSignal <-
      Async.create
        { initial: Nothing
        , load:
            do
              token <- liftEffect $ read tokenSignal
              token
                # maybe (pure Nothing) \t -> do
                    res <- hush <$> Apiary.makeRequest (Apiary.Route :: GetUser) (addBaseUrl <<< addToken t) Apiary.none Apiary.none Apiary.none
                    for res (Variant.match { ok: pure <<< Record.delete (SProxy :: _ "token") <<< _.user })
        , save: const $ pure unit
        }
    Selector.create
      { select:
          do
            token <- Selector.select tokenSignal
            profile <- Selector.select profileSignal
            pure do
              t <- token
              { exp, username } <- hush $ Jwt.decode t
              pure { token: t, username, expirationTime: fromMilliseconds $ Milliseconds $ exp * 1000.0, profile }
      , update:
          \auth -> do
            Selector.write tokenSignal (_.token <$> auth)
            Selector.write profileSignal (_.profile =<< auth)
      }
