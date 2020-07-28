module Conduit.Control.Routing where

import Prelude
import Conduit.Data.Route (Route)
import Control.Monad.Free.Trans (FreeT, liftFreeT)
import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxFunctor, class IxMonad, iapply, ibind, ipure)
import Data.Newtype (class Newtype, unwrap, wrap)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)

data Command a
  = Redirect Route
  | Override Route
  | Continue

derive instance functorCommand :: Functor Command

data Pending

data Completed

newtype Routing i o a
  = Routing (FreeT Command Aff a)

liftCommand :: Command Unit -> Routing Pending Completed Unit
liftCommand = wrap <<< liftFreeT

redirect :: Route -> Routing Pending Completed Unit
redirect = liftCommand <<< Redirect

override :: Route -> Routing Pending Completed Unit
override = liftCommand <<< Override

continue :: Routing Pending Completed Unit
continue = liftCommand Continue

derive instance newtypeRouting :: Newtype (Routing i o a) _

instance ixFunctorRouting :: IxFunctor Routing where
  imap f a = wrap do f <$> unwrap a

instance ixApplyRouting :: IxApply Routing where
  iapply f a = wrap do unwrap f <*> unwrap a

instance ixBindRouting :: IxBind Routing where
  ibind ma f = wrap do unwrap ma >>= unwrap <<< f

instance ixApplicativeRouting :: IxApplicative Routing where
  ipure = wrap <<< pure

instance ixMonadRouting :: IxMonad Routing

derive instance functorRouting :: Functor (Routing Pending Pending)

instance applyRouting :: Apply (Routing Pending Pending) where
  apply = iapply

instance applicativeRouting :: Applicative (Routing Pending Pending) where
  pure = ipure

instance bindRouting :: Bind (Routing Pending Pending) where
  bind = ibind

instance monadRouting :: Monad (Routing Pending Pending)

instance monadEffectRouting :: MonadEffect (Routing Pending Pending) where
  liftEffect = wrap <<< liftEffect

instance monadAffRouting :: MonadAff (Routing Pending Pending) where
  liftAff = wrap <<< liftAff
