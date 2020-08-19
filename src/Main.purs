module Main where

import Prelude
import Conduit.Component.Auth as Auth
import Conduit.Component.Routing as Routing
import Conduit.Data.Route (routeCodec)
import Conduit.Root as Root
import Control.Monad.Reader as Reader
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  container <- getElementById "conduit" =<< (map toNonElementParentNode $ document =<< window)
  case container of
    Nothing -> throw "Conduit container element not found."
    Just c -> do
      authSignal /\ authManager <- Auth.mkAuthManager
      routingSignal /\ routingManager <- Routing.mkRoutingManager routeCodec
      root <- Reader.runReaderT Root.mkRoot { authSignal, routingSignal }
      render
        ( authManager
            ( routingManager
                (root unit)
            )
        )
        c
