module Conduit.Component.Routing where

import Prelude
import Conduit.Data.Route (Route)
import Conduit.Data.Transition (Transition(..))
import Conduit.Effects.Routing as Routing
import Conduit.Env.Routing (RoutingSignal, create)
import Control.Monad.Free.Trans (FreeT, liftFreeT, runFreeT)
import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxFunctor, class IxMonad, iapply, ibind, ipure)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, error, killFiber, launchAff, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import React.Basic.Hooks as React
import Routing.Duplex (RouteDuplex')
import Wire.Event as Event
import Wire.React.Class (modify)

mkRoutingManager ::
  RouteDuplex' Route ->
  (Route -> Routing Pending Completed Unit) ->
  Effect (Tuple RoutingSignal (React.JSX -> React.JSX))
mkRoutingManager routes onNavigate = do
  routingSignal <- create
  fiberRef <- Ref.new Nothing
  previousRouteRef <- Ref.new Nothing
  component <-
    React.component "RoutingManager" \content -> React.do
      React.useEffectOnce do
        Event.subscribe (Routing.onPushState routes) \(Tuple _ route) -> do
          Ref.read fiberRef >>= traverse_ \fiber -> launchAff_ do killFiber (error "Transition cancelled") fiber
          previousRoute <- Ref.read previousRouteRef
          modify routingSignal $ const $ Loading previousRoute route
          let
            finalise r =
              liftEffect do
                Ref.write (Just r) previousRouteRef
                modify routingSignal $ const $ Loaded previousRoute r
          fiber <-
            onNavigate route
              # unwrap
              # runFreeT
                  ( \cmd -> do
                      liftEffect $ Ref.write Nothing fiberRef
                      case cmd of
                        Redirect route' -> Routing.redirect route'
                        Override route' -> finalise route'
                        Continue -> finalise route
                      mempty
                  )
              # launchAff
          Ref.write (Just fiber) fiberRef
      pure content
  pure $ Tuple routingSignal component

-- | Command
data Command a
  = Redirect Route
  | Override Route
  | Continue

derive instance functorCommand :: Functor Command

data Pending

data Completed

newtype Routing i o a
  = Routing (FreeT Command Aff a)

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

-- | Helpers
liftCommand :: Command Unit -> Routing Pending Completed Unit
liftCommand = wrap <<< liftFreeT

redirect :: Route -> Routing Pending Completed Unit
redirect = liftCommand <<< Redirect

override :: Route -> Routing Pending Completed Unit
override = liftCommand <<< Override

continue :: Routing Pending Completed Unit
continue = liftCommand Continue
