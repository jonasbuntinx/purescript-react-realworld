module Conduit.Component.Routing where

import Prelude
import Conduit.Data.Route (Route(..), routeCodec)
import Data.Either (either)
import Effect (Effect)
import Effect.Ref as Ref
import Halogen.Subscription as Subscription
import React.Basic.Hooks as React
import Routing.Duplex (parse, print)
import Routing.PushState as PushState
import Wire.React.Router as Router

mkRoutingManager ::
  Effect
    { read :: Effect Route
    , event :: Subscription.Emitter Route
    , navigate :: Route -> Effect Unit
    , redirect :: Route -> Effect Unit
    , component :: React.JSX
    }
mkRoutingManager = do
  interface <- PushState.makeInterface
  { path } <- interface.locationState
  value <- Ref.new $ either (const Error) identity $ parse routeCodec path
  { emitter, listener } <- Subscription.create
  router <-
    Router.makeRouter interface
      { parse: parse routeCodec
      , print: print routeCodec
      , onRoute: const $ Router.continue
      , onTransition:
          case _ of
            Router.Resolved _ route -> do
              newRoute <- Ref.modify (const route) value
              Subscription.notify listener newRoute
            _ -> pure unit
      }
  pure
    { read: Ref.read value
    , event: emitter
    , navigate: router.navigate
    , redirect: router.redirect
    , component: router.component
    }
