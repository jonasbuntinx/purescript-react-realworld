module Conduit.AppM where

import Prelude
import Conduit.Capability.Auth (class MonadAuth)
import Conduit.Capability.Routing (class MonadRouting)
import Conduit.Data.Jwt as Jwt
import Conduit.Data.Route (Route)
import Conduit.Env (Env)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign.Day (fromMilliseconds)
import Type.Equality (class TypeEquals, from)
import Wire.React.Class (modify, read)

newtype AppM m a
  = AppM (ReaderT Env m a)

runAppM :: forall m. Env -> AppM m ~> m
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor m => Functor (AppM m)

derive newtype instance applyAppM :: Apply m => Apply (AppM m)

derive newtype instance applicativeAppM :: Applicative m => Applicative (AppM m)

derive newtype instance bindAppM :: Bind m => Bind (AppM m)

derive newtype instance monadAppM :: Monad m => Monad (AppM m)

derive newtype instance semigroupAppM :: (Semigroup a, Apply m) => Semigroup (AppM m a)

derive newtype instance monoidAppM :: (Monoid a, Applicative m) => Monoid (AppM m a)

derive newtype instance monadEffectAppM :: MonadEffect m => MonadEffect (AppM m)

derive newtype instance monadAffAppM :: MonadAff m => MonadAff (AppM m)

instance monadAskAppM :: (TypeEquals e Env, Monad m) => MonadAsk e (AppM m) where
  ask = AppM $ asks from

instance monadAuthAppM :: MonadEffect m => MonadAuth (AppM m) where
  read = ask >>= \{ auth } -> liftEffect $ read auth.signal
  login token profile = do
    { auth } <- ask
    liftEffect
      $ modify auth.signal \_ -> do
          { exp, username } <- hush $ Jwt.decode token
          pure { token, username, expirationTime: fromMilliseconds $ Milliseconds $ exp * 1000.0, profile: Just profile }
  logout = ask >>= \{ auth } -> liftEffect $ modify auth.signal $ const Nothing
  updateProfile profile = ask >>= \{ auth } -> liftEffect $ modify auth.signal $ map $ _ { profile = Just profile }

instance monadRoutingAppM :: MonadEffect m => MonadRouting Route (AppM m) where
  navigate route = ask >>= \{ routing } -> liftEffect $ routing.navigate route
  redirect route = ask >>= \{ routing } -> liftEffect $ routing.redirect route
