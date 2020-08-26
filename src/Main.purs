module Main where

import Prelude
import Conduit.Component.Auth as Auth
import Conduit.Component.Routing as Routing
import Conduit.Data.Route (Route(..), routeCodec)
import Conduit.Root as Root
import Control.Monad.Reader (runReaderT)
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
      auth /\ authManager <- Auth.mkAuthManager
      routing /\ routingManager <- Routing.mkRoutingManager routeCodec Error
      root <- runReaderT Root.mkRoot { auth, routing }
      render (authManager (routingManager (root unit))) c
