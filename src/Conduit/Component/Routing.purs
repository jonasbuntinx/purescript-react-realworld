module Conduit.Component.Routing where

import Prelude
import Conduit.Data.Route (Route)
import Conduit.Env.Routing (Action(..), RoutingSignal, create)
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
  interface <- PushState.makeInterface
  routingSignal <- create interface routes
  component <-
    React.component "RoutingManager" \content -> React.do
      React.useEffectOnce do
        Event.subscribe (onPushState interface routes) \route -> do
          modify routingSignal $ const { route, action: NoOp }
      pure content
  pure $ routingSignal /\ component
  where
  onPushState interface matcher =
    Event.makeEvent \k ->
      PushState.matchesWith (parse matcher) (\_ new -> k new) interface
