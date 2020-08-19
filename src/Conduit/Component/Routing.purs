module Conduit.Component.Routing where

import Prelude
import Conduit.Data.Route (Route)
import Conduit.Env.Routing (RoutingSignal, create, pushStateInterface)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import React.Basic.Hooks as React
import Routing.Duplex (RouteDuplex', parse)
import Routing.PushState as PushState
import Wire.Event as Event
import Wire.React.Class (modify)

mkRoutingManager ::
  RouteDuplex' Route ->
  Effect (Tuple RoutingSignal (React.JSX -> React.JSX))
mkRoutingManager routes = do
  routingSignal <- create
  component <-
    React.component "RoutingManager" \content -> React.do
      React.useEffectOnce do
        Event.subscribe (onPushState routes) \(Tuple _ route) -> do
          modify routingSignal $ const $ route
      pure content
  pure $ Tuple routingSignal component
  where
  onPushState matcher =
    Event.makeEvent \k ->
      PushState.matchesWith (parse matcher) (\old new -> k (old /\ new)) pushStateInterface
