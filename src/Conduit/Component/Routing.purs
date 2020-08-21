module Conduit.Component.Routing where

import Prelude
import Conduit.Capability.Routing (toRouteURL)
import Conduit.Data.Route (Route)
import Conduit.Env.Routing (RoutingEnv, create)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Foreign.NullOrUndefined (undefined)
import React.Basic.Hooks as React
import Routing.Duplex (RouteDuplex', parse)
import Routing.PushState as PushState
import Wire.Event as Event
import Wire.React.Class (modify)

mkRoutingManager ::
  RouteDuplex' Route ->
  Effect (RoutingEnv /\ (React.JSX -> React.JSX))
mkRoutingManager routes = do
  interface <- PushState.makeInterface
  routingSignal <- create
  component <-
    React.component "RoutingManager" \content -> React.do
      React.useEffectOnce do
        Event.subscribe (onPushState interface routes) \(_ /\ route) -> do
          modify routingSignal $ const $ route
      pure content
  pure
    $ { signal: routingSignal
      , navigate: navigate interface
      , redirect: redirect interface
      }
    /\ component
  where
  onPushState interface matcher =
    Event.makeEvent \k ->
      PushState.matchesWith (parse matcher) (\old new -> k (old /\ new)) interface

  navigate interface = interface.pushState undefined <<< toRouteURL

  redirect interface = interface.replaceState undefined <<< toRouteURL
