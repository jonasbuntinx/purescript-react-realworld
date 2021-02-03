module Conduit.Component.Routing where

import Prelude
import Conduit.Data.Route (Route(..), routeCodec)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event as Event
import React.Basic.Hooks as React
import Routing.Duplex (parse, print)
import Routing.PushState as PushState
import Wire.React.Router as Router

mkRoutingManager ::
  Effect
    { read :: Effect { route :: Route, prevRoute :: Maybe Route }
    , event :: Event.Event { route :: Route, prevRoute :: Maybe Route }
    , navigate :: Route -> Effect Unit
    , redirect :: Route -> Effect Unit
    , component :: React.JSX
    }
mkRoutingManager = do
  interface <- PushState.makeInterface
  { path } <- interface.locationState
  value <-
    Ref.new
      { route: either (const Error) identity $ parse routeCodec path
      , prevRoute: Nothing
      }
  { event, push } <- Event.create
  router <-
    Router.makeRouter interface
      { parse: parse routeCodec
      , print: print routeCodec
      , onRoute: const $ Router.continue
      , onTransition:
          case _ of
            Router.Resolved prevRoute route -> do
              newValue <- Ref.modify (const $ { route, prevRoute }) value
              push newValue
            _ -> pure unit
      }
  pure
    { read: Ref.read value
    , event: event
    , navigate: router.navigate
    , redirect: router.redirect
    , component: router.component
    }
