module Conduit.Component.Routing where

import Prelude
import Conduit.Control.Routing (Command(..), Completed, Pending, Routing)
import Conduit.Data.Route (Route)
import Conduit.Data.Transition (Transition(..))
import Conduit.Effects.Routing as Routing
import Conduit.Env.Routing (RoutingSignal, create)
import Control.Monad.Free.Trans (runFreeT)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (error, killFiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
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
