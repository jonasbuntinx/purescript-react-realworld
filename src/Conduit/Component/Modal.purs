module Conduit.Component.Modal where

import Prelude
import Conduit.Component.Portal as Portal
import Data.Foldable (elem, traverse_)
import Data.Monoid (guard)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.JSS (JSS, jss)
import React.Basic.DOM as R
import React.Basic.DOM.Components.GlobalEvents (defaultOptions, windowEvent)
import React.Basic.Events (handler_)
import React.Basic.Hooks as React
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes as KeyboardEventTypes

type Props
  = { isOpen :: Boolean
    , onEscapeKey :: Effect Unit
    , onOverlayClick :: Effect Unit
    , children :: Array React.JSX
    }

modal :: (Props -> Props) -> React.JSX
modal =
  let
    defaultProps =
      { isOpen: false
      , onEscapeKey: pure unit
      , onOverlayClick: pure unit
      , children: []
      }

    component = unsafePerformEffect mkModal
  in
    \fn -> component (fn defaultProps)

mkModal :: React.Component Props
mkModal = do
  Portal.portal "Modal" \props -> React.do
    pure $ guard props.isOpen
      $ windowEvent
          { eventType: KeyboardEventTypes.keydown
          , handler:
              KeyboardEvent.fromEvent
                >>> traverse_ \event ->
                    guard (KeyboardEvent.key event `elem` [ "Escape", "Esc" ]) do
                      props.onEscapeKey
          , options: defaultOptions
          }
      $ root
          [ overlay props
          , React.fragment props.children
          ]
  where
  root children =
    R.div
      { className: "modal-container"
      , children
      }

  overlay props =
    R.div
      { className: "modal-overlay"
      , onClick: handler_ props.onOverlayClick
      }

styles :: JSS
styles =
  jss
    { "@global":
        { ".modal-overlay":
            { position: "absolute"
            , width: "100%"
            , height: "100%"
            , background: "#1a202c"
            , opacity: 0.5
            , zIndex: -1
            }
        , ".modal-container":
            { position: "fixed"
            , width: "100%"
            , height: "100%"
            , top: "0"
            , left: "0"
            , display: "flex"
            , alignItems: "center"
            , justifyContent: "center"
            , zIndex: 50
            }
        }
    }
