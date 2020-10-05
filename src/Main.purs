module Main where

import Prelude
import Conduit.Component.Auth as Auth
import Conduit.Data.Route (Route(..), routeCodec)
import Conduit.Root as Root
import Control.Monad.Reader (runReaderT)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic as React
import React.Basic.DOM (render)
import Routing.Duplex (parse, print)
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
      auth <- Auth.makeAuthManager
      interface <- PushState.makeInterface
      router <-
        Router.makeRouter interface
          { fallback: Error
          , parse: parse routeCodec
          , print: print routeCodec
          , onRoute: const $ Router.continue
          }
      root <-
        runReaderT Root.makeRoot
          { auth: { signal: auth.signal }
          , router: { signal: router.signal, navigate: router.navigate, redirect: router.redirect }
          }
      render (React.fragment [ router.component, auth.component, root unit ]) c
