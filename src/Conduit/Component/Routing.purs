module Conduit.Component.Routing where

import Prelude
import Conduit.Capability.Routing (class IsRoute, toRouteURL)
import Data.Either (hush)
import Data.Maybe (fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Foreign.NullOrUndefined (undefined)
import React.Basic.Hooks as React
import Routing.Duplex (RouteDuplex', parse)
import Routing.PushState as PushState
import Wire.Event as Event
import Wire.React.Class (modify)
import Wire.React.Pure as Pure

type RoutingEnv route
  = { signal :: Pure.Pure route
    , navigate :: route -> Effect Unit
    , redirect :: route -> Effect Unit
    }

mkRoutingManager ::
  forall route.
  IsRoute route =>
  RouteDuplex' route ->
  route ->
  Effect ((RoutingEnv route) /\ (React.JSX -> React.JSX))
mkRoutingManager routes default = do
  interface <- PushState.makeInterface
  location <- interface.locationState
  routingSignal <- Pure.create $ fromMaybe default $ hush $ parse routes location.path
  component <-
    React.component "RoutingManager" \content -> React.do
      React.useEffectOnce do
        Event.subscribe (onPushState interface routes) \route -> do
          modify routingSignal $ const route
      pure content
  pure
    $ { signal: routingSignal
      , navigate: interface.pushState undefined <<< toRouteURL
      , redirect: interface.replaceState undefined <<< toRouteURL
      }
    /\ component
  where
  onPushState interface matcher =
    Event.makeEvent \k ->
      PushState.matchesWith (parse matcher) (\_ new -> k new) interface
