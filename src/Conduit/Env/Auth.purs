module Conduit.Env.Auth where

import Prelude
import Apiary.Client (makeRequest) as Apiary
import Apiary.Route (Route(..)) as Apiary
import Apiary.Types (none) as Apiary
import Conduit.Api.User (GetUser)
import Conduit.Api.Utils (addBaseUrl, addToken)
import Conduit.Data.Jwt as Jwt
import Conduit.Data.Profile (Profile)
import Conduit.Data.Username (Username)
import Conduit.Effects.LocalStorage as LocalStorage
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Either (hush)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Seconds(..))
import Data.Traversable (for)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign.Moment (Moment, unix)
import Record as Record
import Wire.React.Async as Async
import Wire.React.Class as Wire
import Wire.React.Selector as Selector
import Wire.React.Sync as Sync

type Auth
  = { token :: String
    , username :: Username
    , expirationTime :: Moment
    , profile :: Maybe Profile
    }

type AuthSignal
  = Selector.Selector (Maybe Auth)

create :: Effect AuthSignal
create = do
  tokenSignal <-
    Sync.create
      { load: LocalStorage.read cachedToken
      , save:
          case _ of
            Nothing -> LocalStorage.delete cachedToken
            Just token -> LocalStorage.write cachedToken token
      }
  profileSignal <-
    Async.create
      { initial: Nothing
      , load:
          do
            token <- liftEffect $ Wire.read tokenSignal
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
            pure
              { token: t
              , username
              , expirationTime: unix $ Seconds exp
              , profile
              }
    , update:
        \auth -> do
          Selector.write tokenSignal (_.token <$> auth)
          Selector.write profileSignal (_.profile =<< auth)
    }

-- | Local Storage
cachedToken :: LocalStorage.StorageKey String
cachedToken = LocalStorage.StorageKey "token"

-- | Helpers
login ::
  forall m r.
  MonadAsk { authSignal :: AuthSignal | r } m =>
  MonadEffect m =>
  String ->
  Profile ->
  m Unit
login token profile = do
  { authSignal } <- ask
  liftEffect $ login' authSignal token profile

login' :: AuthSignal -> String -> Profile -> Effect Unit
login' authSignal token profile =
  Wire.modify authSignal \_ -> do
    { exp, username } <- hush $ Jwt.decode token
    pure
      { token
      , username
      , expirationTime: unix $ Seconds exp
      , profile: Just profile
      }

refreshToken ::
  forall m r.
  MonadAsk { authSignal :: AuthSignal | r } m =>
  MonadEffect m =>
  String ->
  m Unit
refreshToken token = do
  { authSignal } <- ask
  liftEffect $ refreshToken' authSignal token

refreshToken' :: AuthSignal -> String -> Effect Unit
refreshToken' authSignal token = Wire.modify authSignal $ map $ _ { token = token }

logout ::
  forall m r.
  MonadAsk { authSignal :: AuthSignal | r } m =>
  MonadEffect m =>
  m Unit
logout = do
  { authSignal } <- ask
  liftEffect $ logout' authSignal

logout' :: AuthSignal -> Effect Unit
logout' authSignal = do
  Wire.modify authSignal $ const Nothing

updateProfile ::
  forall m r.
  MonadAsk { authSignal :: AuthSignal | r } m =>
  MonadEffect m =>
  Profile ->
  m Unit
updateProfile profile = do
  { authSignal } <- ask
  liftEffect $ Wire.modify authSignal $ map $ _ { profile = Just profile }
