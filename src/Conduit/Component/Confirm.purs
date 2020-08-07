module Conduit.Component.Confirm where

import Prelude
import Conduit.Component.Modal as Modal
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks as React

type Props
  = { isOpen :: Boolean
    , title :: String
    , confirmText :: String
    , cancelText :: String
    , onConfirm :: Effect Unit
    , onCancel :: Effect Unit
    , children :: Array React.JSX
    }

confirm :: (Props -> Props) -> React.JSX
confirm =
  let
    defaultProps =
      { isOpen: false
      , title: ""
      , confirmText: "Confirm"
      , cancelText: "Cancel"
      , onConfirm: pure unit
      , onCancel: pure unit
      , children: []
      }

    component = unsafePerformEffect mkConfirm
  in
    \fn -> component (fn defaultProps)

mkConfirm :: React.Component Props
mkConfirm = do
  React.component "Confirm" \props -> React.do
    pure
      $ Modal.modal
          _
            { isOpen = props.isOpen
            , onEscapeKey = props.onCancel
            , onOverlayClick = props.onCancel
            , children =
              [ R.div
                  { className: "card"
                  , children:
                      [ R.div
                          { className: "card-header"
                          , children:
                              [ R.text props.title ]
                          }
                      , R.div
                          { className: "card-block"
                          , children: props.children
                          }
                      , R.div
                          { className: "card-footer"
                          , children:
                              [ R.span
                                  { className: "pull-xs-right"
                                  , children:
                                      [ R.button
                                          { className: "btn btn-sm btn-outline-secondary"
                                          , onClick: handler_ $ props.onCancel
                                          , children:
                                              [ R.text props.cancelText
                                              ]
                                          }
                                      , R.text " "
                                      , R.button
                                          { className: "btn btn-sm btn-primary"
                                          , onClick: handler_ $ props.onConfirm
                                          , children:
                                              [ R.text props.confirmText
                                              ]
                                          }
                                      ]
                                  }
                              ]
                          }
                      ]
                  }
              ]
            }
