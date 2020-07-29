module Conduit.State.User where

import Prelude
import Apiary.Route (Route(..)) as Apiary
import Apiary.Types (none) as Apiary
import Conduit.Api.User (GetUser)
import Conduit.Api.Utils as Utils
import Conduit.Data.Profile (Profile)
import Conduit.Effects.LocalStorage as LocalStorage
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Either (hush)
import Data.Maybe (Maybe(..), maybe)
import Data.Moment (Moment, now)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Variant as Variant
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Record as Record
import Wire.React.Async as Async
import Wire.React.Class (modify, read)
import Wire.React.Selector as Selector
import Wire.React.Sync as Sync

type Auth
  = { token :: String
    , updated :: Moment
    }

type User
  = Tuple (Maybe Auth) (Maybe Profile)

type UserState
  = Selector.Selector User

create :: Effect UserState
create = do
  authState <-
    Sync.create
      { load: LocalStorage.read cachedAuth
      , save:
          case _ of
            Nothing -> LocalStorage.delete cachedAuth
            Just auth -> LocalStorage.write cachedAuth auth
      }
  profileState <-
    Async.create
      { initial: Nothing
      , load:
          do
            auth <- liftEffect $ read authState
            auth
              # maybe (pure Nothing) \{ token } -> do
                  res <- hush <$> Utils.makeSecureRequest' token (Apiary.Route :: GetUser) Apiary.none Apiary.none Apiary.none
                  pure (res >>= Variant.prj (SProxy :: _ "ok") >>> map _.user >>> map (Record.delete (SProxy :: _ "token")))
      , save: const $ pure unit
      }
  Selector.create
    { select:
        do
          auth <- Selector.select authState
          profile <- Selector.select profileState
          pure $ Tuple auth profile
    , update:
        \(Tuple auth profile) -> do
          Selector.modify authState $ const auth
          Selector.modify profileState $ const profile
    }

-- | Local Storage
cachedAuth :: LocalStorage.StorageKey Auth
cachedAuth = LocalStorage.StorageKey "auth"

-- | Helpers
login ::
  forall m r.
  MonadAsk { userState :: UserState | r } m =>
  MonadEffect m =>
  String ->
  Profile ->
  m Unit
login token profile = do
  { userState } <- ask
  liftEffect $ login' userState token profile

login' :: UserState -> String -> Profile -> Effect Unit
login' userState token profile = do
  now <- now
  modify userState \_ -> Tuple (Just { token, updated: now }) (Just profile)

refreshToken ::
  forall m r.
  MonadAsk { userState :: UserState | r } m =>
  MonadEffect m =>
  String ->
  m Unit
refreshToken token = do
  { userState } <- ask
  liftEffect $ refreshToken' userState token

refreshToken' :: UserState -> String -> Effect Unit
refreshToken' userState token = do
  now <- now
  modify userState \(Tuple _ profile) -> Tuple (Just { token, updated: now }) profile

logout ::
  forall m r.
  MonadAsk { userState :: UserState | r } m =>
  MonadEffect m =>
  m Unit
logout = do
  { userState } <- ask
  liftEffect $ logout' userState

logout' :: UserState -> Effect Unit
logout' userState = do
  modify userState \_ -> Tuple Nothing Nothing
