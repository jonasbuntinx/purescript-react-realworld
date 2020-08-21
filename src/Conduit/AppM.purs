module Conduit.AppM where

import Prelude
import Conduit.Capability.Auth (class MonadAuth)
import Conduit.Capability.Routing (class MonadRouting)
import Conduit.Data.Route (Route)
import Conduit.Env (Env)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Type.Equality (class TypeEquals, from, to)

newtype AppM a
  = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance applicativeAppM :: Applicative AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance monadAuthAppM :: MonadAuth AppM where
  read = ask >>= \env -> liftEffect (to env).auth.read
  login token profile = ask >>= \env -> liftEffect ((to env).auth.login token profile)
  logout = ask >>= \env -> liftEffect ((to env).auth.logout)
  updateProfile profile = ask >>= \env -> liftEffect ((to env).auth.updateProfile profile)

instance monadRoutingAppM :: MonadRouting Route AppM where
  navigate route = ask >>= \env -> liftEffect ((to env).routing.navigate route)
  redirect route = ask >>= \env -> liftEffect ((to env).routing.redirect route)
