module Conduit.Component.Link where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import React.Basic.DOM as R
import React.Basic.DOM.Events (altKey, button, ctrlKey, metaKey, preventDefault, shiftKey, stopPropagation)
import React.Basic.Events (handler, merge, syntheticEvent)
import React.Basic.Hooks as React

type Props
  = { className :: String
    , href :: String
    , onClick :: Effect Unit
    , children :: Array React.JSX
    }

link :: Props -> React.JSX
link props =
  R.a
    { className: props.className
    , href: props.href
    , onClick:
        handler (merge { button, metaKey, altKey, ctrlKey, shiftKey, syntheticEvent }) \{ button, metaKey, altKey, ctrlKey, shiftKey, syntheticEvent } -> do
          case button, metaKey, altKey, ctrlKey, shiftKey of
            Just 0, Just false, Just false, Just false, Just false ->
              runEffectFn1
                (handler (stopPropagation <<< preventDefault) $ \_ -> props.onClick)
                syntheticEvent
            _, _, _, _, _ ->
              runEffectFn1
                (handler stopPropagation mempty)
                syntheticEvent
    , children: props.children
    }
