module Conduit.Components.Portal (portal) where

import Prelude
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import React.Basic.DOM as R
import React.Basic.Hooks as React
import Web.DOM.Document as DOMDocument
import Web.DOM.Element as DOMElement
import Web.DOM.Element as Element
import Web.DOM.Node as DOMNode
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as HTMLWindow

portal ::
  forall hooks props.
  String ->
  (props -> React.Render Unit hooks React.JSX) ->
  React.Component props
portal name renderFn =
  React.component name \props -> React.do
    jsx <- renderFn props
    parent /\ setParent <- React.useState Nothing
    React.useEffectOnce do
      doc <- HTMLWindow.document =<< HTML.window
      el <- DOMDocument.createElement "div" (HTMLDocument.toDocument doc)
      DOMElement.setAttribute "data-portal" name el
      applyToBody DOMNode.appendChild el doc
      setParent \_ -> pure el
      pure do
        applyToBody DOMNode.removeChild el doc
    pure case parent of
      Nothing -> React.empty
      Just el -> R.createPortal jsx el
  where
  applyToBody f el doc =
    HTMLDocument.body doc
      >>= traverse_ \body ->
          f (Element.toNode el) (HTMLElement.toNode body)
