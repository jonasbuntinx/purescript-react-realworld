module Conduit.Component.Routing where

import Prelude
import Conduit.Data.Route (Route, toRouteString)
import Conduit.Env.Routing (RoutingSignal, create)
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
  Effect
    ( { routingSignal :: RoutingSignal
      , navigate :: Route -> Effect Unit
      , redirect :: Route -> Effect Unit
      }
        /\ (React.JSX -> React.JSX)
    )
mkRoutingManager routes = do
  interface <- PushState.makeInterface
  routingSignal <- create
  component <-
    React.component "RoutingManager" \content -> React.do
      React.useEffectOnce do
        Event.subscribe (onPushState interface routes) \(_ /\ route) -> do
          modify routingSignal $ const $ route
      pure content
  pure $ { routingSignal, navigate: navigate interface, redirect: redirect interface } /\ component
  where
  onPushState interface matcher =
    Event.makeEvent \k ->
      PushState.matchesWith (parse matcher) (\old new -> k (old /\ new)) interface

  navigate interface = interface.pushState undefined <<< toRouteString

  redirect interface = interface.replaceState undefined <<< toRouteString
