module Conduit.Component.Routing where

import Prelude
import Conduit.Data.Route (Route(..), routeCodec)
import Data.Either (either)
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event as Event
import React.Basic.Hooks as React
import Routing.Duplex (parse, print)
import Routing.PushState as PushState
import Wire.React.Router as Router

mkRoutingManager ::
  Effect
    { read :: Effect Route
    , event :: Event.Event Route
    , navigate :: Route -> Effect Unit
    , redirect :: Route -> Effect Unit
    , component :: React.JSX
    }
mkRoutingManager = do
  interface <- PushState.makeInterface
  { path } <- interface.locationState
  value <- Ref.new $ either (const Error) identity $ parse routeCodec path
  event <- Event.create
  router <-
    Router.makeRouter interface
      { parse: parse routeCodec
      , print: print routeCodec
      , onRoute: const $ Router.continue
      , onTransition:
          case _ of
            Router.Resolved _ route -> do
              newRoute <- Ref.modify (const route) value
              event.push newRoute
            _ -> pure unit
      }
  pure
    { read: Ref.read value
    , event: event.event
    , navigate: router.navigate
    , redirect: router.redirect
    , component: router.component
    }
