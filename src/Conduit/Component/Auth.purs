module Conduit.Component.Auth where

import Prelude
import Conduit.Api.User (Token)
import Conduit.Effects.LocalStorage as LocalStorage
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Maybe (Maybe(..))
import Data.Moment (Moment, now)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import React.Basic.Hooks as React
import Wire.React.Class (modify)
import Wire.React.Sync (Sync, create) as Sync

type Auth
  = { token :: Token
    , updated :: Moment
    }

type AuthAtom
  = Sync.Sync (Maybe Auth)

-- | Manager
mkAuthManager ::
  Effect (Tuple AuthAtom (React.JSX -> React.JSX))
mkAuthManager = do
  authAtom <-
    Sync.create
      { load: LocalStorage.read cachedAuth
      , save:
          case _ of
            Nothing -> LocalStorage.delete cachedAuth
            Just auth -> LocalStorage.write cachedAuth auth
      }
  component <-
    React.component "AuthManager" \content -> React.do
      pure content
  pure $ Tuple authAtom component

-- | Local Storage
cachedAuth :: LocalStorage.StorageKey Auth
cachedAuth = LocalStorage.StorageKey "auth"

-- | Helpers
login ::
  forall m r.
  MonadAsk { authAtom :: AuthAtom | r } m =>
  MonadEffect m =>
  String -> m Unit
login token = do
  { authAtom } <- ask
  liftEffect do
    now <- now
    modify authAtom \_ -> Just { token, updated: now }

logout ::
  forall m r.
  MonadAsk { authAtom :: AuthAtom | r } m =>
  MonadEffect m =>
  m Unit
logout = do
  { authAtom } <- ask
  liftEffect do
    modify authAtom \_ -> Nothing
