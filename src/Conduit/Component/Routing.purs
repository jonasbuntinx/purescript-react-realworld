module Conduit.Component.Routing where

import Prelude
import Conduit.Data.Route (Route)
import Conduit.Env.Routing (RoutingSignal, create, pushStateInterface)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import React.Basic.Hooks as React
import Routing.Duplex (RouteDuplex', parse)
import Routing.PushState as PushState
import Wire.Event as Event
import Wire.React.Class (modify)

mkRoutingManager ::
  RouteDuplex' Route ->
  Effect (RoutingSignal /\ (React.JSX -> React.JSX))
mkRoutingManager routes = do
  routingSignal <- create
  component <-
    React.component "RoutingManager" \content -> React.do
      React.useEffectOnce do
        Event.subscribe (onPushState routes) \(_ /\ route) -> do
          modify routingSignal $ const $ route
      pure content
  pure $ routingSignal /\ component
  where
  onPushState matcher =
    Event.makeEvent \k ->
      PushState.matchesWith (parse matcher) (\old new -> k (old /\ new)) pushStateInterface
