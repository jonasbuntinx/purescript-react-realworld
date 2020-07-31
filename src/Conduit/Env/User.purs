module Conduit.Env.User where

import Prelude
import Apiary.Client (makeRequest) as Apiary
import Apiary.Route (Route(..)) as Apiary
import Apiary.Types (none) as Apiary
import Conduit.Api.User (GetUser)
import Conduit.Api.Utils (addBaseUrl, addToken)
import Conduit.Data.Profile (Profile)
import Conduit.Effects.LocalStorage as LocalStorage
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Either (hush)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Variant as Variant
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign.Moment (Moment, now)
import Record as Record
import Wire.React.Async as Async
import Wire.React.Class as Wire
import Wire.React.Selector as Selector
import Wire.React.Sync as Sync

type Auth
  = { token :: String
    , updated :: Moment
    }

type User
  = Tuple (Maybe Auth) (Maybe Profile)

type UserSignal
  = Selector.Selector User

create :: Effect UserSignal
create = do
  authSignal <-
    Sync.create
      { load: LocalStorage.read cachedAuth
      , save:
          case _ of
            Nothing -> LocalStorage.delete cachedAuth
            Just auth -> LocalStorage.write cachedAuth auth
      }
  profileSignal <-
    Async.create
      { initial: Nothing
      , load:
          do
            auth <- liftEffect $ Wire.read authSignal
            auth
              # maybe (pure Nothing) \{ token } -> do
                  res <- hush <$> Apiary.makeRequest (Apiary.Route :: GetUser) (addBaseUrl <<< addToken token) Apiary.none Apiary.none Apiary.none
                  pure (res >>= Variant.prj (SProxy :: _ "ok") >>> map _.user >>> map (Record.delete (SProxy :: _ "token")))
      , save: const $ pure unit
      }
  Selector.create
    { select:
        do
          auth <- Selector.select authSignal
          profile <- Selector.select profileSignal
          pure $ Tuple auth profile
    , update:
        \(Tuple auth profile) -> do
          Selector.write authSignal auth
          Selector.write profileSignal profile
    }

-- | Local Storage
cachedAuth :: LocalStorage.StorageKey Auth
cachedAuth = LocalStorage.StorageKey "auth"

-- | Helpers
login ::
  forall m r.
  MonadAsk { userSignal :: UserSignal | r } m =>
  MonadEffect m =>
  String ->
  Profile ->
  m Unit
login token profile = do
  { userSignal } <- ask
  liftEffect $ login' userSignal token profile

login' :: UserSignal -> String -> Profile -> Effect Unit
login' userSignal token profile = do
  now <- now
  Wire.modify userSignal \_ -> Tuple (Just { token, updated: now }) (Just profile)

refreshToken ::
  forall m r.
  MonadAsk { userSignal :: UserSignal | r } m =>
  MonadEffect m =>
  String ->
  m Unit
refreshToken token = do
  { userSignal } <- ask
  liftEffect $ refreshToken' userSignal token

refreshToken' :: UserSignal -> String -> Effect Unit
refreshToken' userSignal token = do
  now <- now
  Wire.modify userSignal \(Tuple _ profile) -> Tuple (Just { token, updated: now }) profile

logout ::
  forall m r.
  MonadAsk { userSignal :: UserSignal | r } m =>
  MonadEffect m =>
  m Unit
logout = do
  { userSignal } <- ask
  liftEffect $ logout' userSignal

logout' :: UserSignal -> Effect Unit
logout' userSignal = do
  Wire.modify userSignal \_ -> Tuple Nothing Nothing

updateProfile ::
  forall m r.
  MonadAsk { userSignal :: UserSignal | r } m =>
  MonadEffect m =>
  Profile ->
  m Unit
updateProfile profile = do
  { userSignal } <- ask
  liftEffect $ Wire.modify userSignal \(Tuple auth _) -> Tuple auth (Just profile)
