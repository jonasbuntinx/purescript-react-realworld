module Conduit.Env.Auth where

import Prelude
import Apiary.Client (makeRequest) as Apiary
import Apiary.Route (Route(..)) as Apiary
import Apiary.Types (none) as Apiary
import Conduit.Api.Endpoints (GetUser)
import Conduit.Api.Utils (addBaseUrl, addToken)
import Conduit.Data.Auth (Auth)
import Conduit.Data.Jwt as Jwt
import Conduit.Data.Profile (UserProfile)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Either (hush)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign.Day (fromMilliseconds)
import Foreign.Generic (decodeJSON, encodeJSON)
import Record as Record
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.Storage.Storage as Storage
import Wire.React.Async as Async
import Wire.React.Class as Wire
import Wire.React.Selector as Selector
import Wire.React.Sync as Sync

type AuthSignal
  = Selector.Selector (Maybe Auth)

create :: Effect AuthSignal
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
            pure { token: t, username, expirationTime: fromMilliseconds $ Milliseconds $ exp * 1000.0, profile }
    , update:
        \auth -> do
          Selector.write tokenSignal (_.token <$> auth)
          Selector.write profileSignal (_.profile =<< auth)
    }

-- | Helpers
login :: forall m r. MonadAsk { login :: String -> UserProfile -> Effect Unit | r } m => MonadEffect m => String -> UserProfile -> m Unit
login token profile = ask >>= \env -> liftEffect $ env.login token profile

logout :: forall m r. MonadAsk { logout :: Effect Unit | r } m => MonadEffect m => m Unit
logout = ask >>= \env -> liftEffect env.logout

updateProfile :: forall m r. MonadAsk { updateProfile :: UserProfile -> Effect Unit | r } m => MonadEffect m => UserProfile -> m Unit
updateProfile profile = ask >>= \env -> liftEffect $ env.updateProfile profile
