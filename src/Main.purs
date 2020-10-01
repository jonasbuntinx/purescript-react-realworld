module Main where

import Prelude
import Conduit.Capability.Routing (toRouteURL)
import Conduit.Component.Auth as Auth
import Conduit.Data.Route (Route(..), routeCodec)
import Conduit.Root as Root
import Control.Monad.Reader (runReaderT)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic as React
import React.Basic.DOM (render)
import Routing.Duplex (parse)
import Routing.PushState as PushState
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)
import Wire.React.Router as Router

main :: Effect Unit
main = do
  container <- getElementById "conduit" =<< (map toNonElementParentNode $ document =<< window)
  case container of
    Nothing -> throw "Conduit container element not found."
    Just c -> do
      { signal: authSignal, authManager } <- Auth.mkAuthManager
      interface <- PushState.makeInterface
      location <- interface.locationState
      { signal: routerSignal, router, navigate, redirect } <-
        Router.makeRouter
          { interface
          , initial: fromMaybe Error $ hush $ parse routeCodec location.path
          , decode: parse routeCodec
          , encode: toRouteURL
          , onRouteChange: const $ Router.continue
          }
      root <- runReaderT Root.mkRoot { auth: { signal: authSignal }, router: { signal: routerSignal, navigate, redirect } }
      render (React.fragment [ router, authManager, root unit ]) c
