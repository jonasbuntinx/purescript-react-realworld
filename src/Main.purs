module Main where

import Prelude
import Conduit.Data.Route (routeCodec)
import Conduit.HOC.Routing as Routing
import Conduit.HOC.Toast as Toast
import Conduit.HOC.User as User
import Conduit.Root as Root
import Control.Monad.Reader as Reader
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception (throw)
import Foreign.JSS as JSS
import React.Basic.DOM (render)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  attachGlobalComponentStyles
  container <- getElementById "conduit" =<< (map toNonElementParentNode $ document =<< window)
  case container of
    Nothing -> throw "Conduit container element not found."
    Just c -> do
      userSignal /\ userManager <- User.mkUserManager
      routingSignal /\ routingManager <- Routing.mkRoutingManager routeCodec (Root.onNavigate userSignal)
      toastSignal /\ toastManager <- Toast.mkToastManager
      root <- Reader.runReaderT Root.mkRoot { userSignal, routingSignal, toastSignal }
      render
        ( userManager
            ( routingManager
                ( toastManager
                    (root unit)
                )
            )
        )
        c

attachGlobalComponentStyles :: Effect Unit
attachGlobalComponentStyles = do
  jssInstance <- JSS.createInstance JSS.preset
  traverse_ (JSS.globalAttachStyleSheet <=< JSS.createStyleSheet jssInstance)
    [ Toast.styles
    ]
