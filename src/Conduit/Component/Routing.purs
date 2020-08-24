module Conduit.Component.Routing where

import Prelude
import Conduit.Capability.Routing (toRouteURL)
import Conduit.Data.Route (Route(..))
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

type RoutingEnv
  = { signal :: Pure.Pure Route
    , navigate :: Route -> Effect Unit
    , redirect :: Route -> Effect Unit
    }

mkRoutingManager ::
  RouteDuplex' Route ->
  Effect (RoutingEnv /\ (React.JSX -> React.JSX))
mkRoutingManager routes = do
  interface <- PushState.makeInterface
  location <- interface.locationState
  routingSignal <- Pure.create $ fromMaybe Error $ hush $ parse routes location.path
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
