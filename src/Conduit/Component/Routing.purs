module Conduit.Component.Routing where

import Prelude
import Conduit.Data.Route (Route(..), routeCodec)
import Data.Either (either)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Ref as Ref
import Halogen.Subscription as HS
import React.Basic.Hooks as React
import Routing.Duplex (parse, print)
import Routing.PushState as PushState
import Web.Router as Router
import Web.Router.Driver.PushState as Driver

type RoutingIO =
  { read :: Effect Route
  , emitter :: HS.Emitter Route
  , navigate :: Route -> Effect Unit
  , redirect :: Route -> Effect Unit
  }

mkRoutingManager :: Effect (RoutingIO /\ React.JSX)
mkRoutingManager = do
  interface <- PushState.makeInterface
  { path } <- interface.locationState
  value <- Ref.new $ either (const Error) identity $ parse routeCodec path
  { emitter, listener } <- HS.create
  let
    driver = Driver.makeDriver_ (parse routeCodec) (print routeCodec) interface
  router <-
    Router.makeRouter
      (\_ _ -> Router.continue)
      ( case _ of
          Router.Resolved _ route -> do
            newRoute <- Ref.modify (const route) value
            HS.notify listener newRoute
          _ -> pure unit
      )
      driver
  component <-
    React.component "Router" \_ -> React.do
      React.useEffectOnce do
        router.initialize
      pure React.empty
  pure
    ( { read: Ref.read value
      , emitter
      , navigate: router.navigate
      , redirect: router.redirect
      }
        /\ component unit
    )
