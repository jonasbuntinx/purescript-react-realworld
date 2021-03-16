module Main where

import Prelude
import Conduit.AppM (runAppM)
import Conduit.Component.Auth as Auth
import Conduit.Component.Routing as Routing
import Conduit.Root as Root
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import React.Basic.DOM (render)
import React.Basic.Hooks as React
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  container <- getElementById "conduit" =<< (map toNonElementParentNode $ document =<< window)
  case container of
    Nothing -> Exception.throw "Conduit container element not found."
    Just c -> do
      auth /\ authManager <- Auth.mkAuthManager
      routing /\ routingManager <- Routing.mkRoutingManager
      launchAff_ do
        root <- runAppM { auth, routing } Root.mkRoot
        liftEffect $ render (React.fragment [ routingManager, authManager, root unit ]) c
