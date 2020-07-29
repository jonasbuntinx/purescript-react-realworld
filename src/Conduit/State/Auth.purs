module Conduit.State.Auth where

import Prelude
import Conduit.Data.Auth (Auth)
import Conduit.Effects.LocalStorage as LocalStorage
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Maybe (Maybe(..))
import Data.Moment (now)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Wire.React.Class (modify)
import Wire.React.Sync (Sync, create) as Sync

type AuthState
  = Sync.Sync (Maybe Auth)

create :: Effect AuthState
create =
  Sync.create
    { load: LocalStorage.read cachedAuth
    , save:
        case _ of
          Nothing -> LocalStorage.delete cachedAuth
          Just auth -> LocalStorage.write cachedAuth auth
    }

-- | Local Storage
cachedAuth :: LocalStorage.StorageKey Auth
cachedAuth = LocalStorage.StorageKey "auth"

-- | Helpers
login ::
  forall m r.
  MonadAsk { authState :: AuthState | r } m =>
  MonadEffect m =>
  String -> m Unit
login token = do
  { authState } <- ask
  liftEffect do
    now <- now
    modify authState \_ -> Just { token, updated: now }

logout ::
  forall m r.
  MonadAsk { authState :: AuthState | r } m =>
  MonadEffect m =>
  m Unit
logout = do
  { authState } <- ask
  liftEffect do
    modify authState \_ -> Nothing
