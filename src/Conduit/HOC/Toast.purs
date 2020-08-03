module Conduit.HOC.Toast where

import Prelude
import Conduit.Component.Portal as Portal
import Conduit.Env.Toast (ToastSignal, create)
import Data.Array (drop, head)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Foreign.JSS (JSS, jss)
import React.Basic.DOM as R
import React.Basic.Hooks as React
import Wire.React.Class (modify, read)

data ToastStatus
  = Empty
  | Visible
  | Expired

transitionDuration :: Milliseconds
transitionDuration = Milliseconds 150.0

mkToastManager :: Effect (Tuple ToastSignal (React.JSX -> React.JSX))
mkToastManager = do
  toastSignal <- create
  component <-
    Portal.portal "ToastManager" \content -> React.do
      state /\ setState <- React.useState { toast: Nothing, status: Empty }
      React.useEffectOnce do
        readQueue toastSignal state setState
        mempty
      pure $ render state content
  pure $ Tuple toastSignal component
  where
  readQueue toastSignal state setState =
    launchAff_ do
      q <- liftEffect $ read toastSignal
      liftEffect $ modify toastSignal $ const $ (drop 1 q)
      delay transitionDuration
      case head q of
        Nothing -> do
          delay $ Milliseconds 500.0
          liftEffect $ readQueue toastSignal state setState
        Just m -> do
          liftEffect $ newMessage state setState m
          delay transitionDuration
          liftEffect $ setState setVisible
          delay m.duration
          liftEffect $ setState setExpired
          delay transitionDuration
          liftEffect $ setState reset
          delay transitionDuration
          liftEffect $ readQueue toastSignal state setState

  newMessage state setState m = do
    unless (Just m == state.toast) do
      setState _ { toast = Just m, status = Empty }

  setVisible = _ { status = Visible }

  setExpired = _ { status = Expired }

  reset = _ { toast = Nothing, status = Empty }

  render state content =
    React.fragment
      [ state.toast
          # maybe React.empty \{ message } ->
              R.div
                { className: "toast-anchor"
                , children:
                    [ R.div
                        { className:
                            "toast-wrapper "
                              <> case state.status of
                                  Empty -> "empty"
                                  Visible -> "visible"
                                  Expired -> "expired"
                        , children:
                            [ R.div
                                { className: "toast-bubble"
                                , children:
                                    [ R.text message ]
                                }
                            ]
                        }
                    ]
                }
      , content
      ]

-- | Style
styles :: JSS
styles =
  jss
    { "@global":
        { ".toast-bubble":
            { "box-sizing": "border-box"
            , "display": "flex"
            , "padding": "16px 24px"
            , "color": "white"
            , "background-color": "black"
            , "border-radius": ".25rem"
            , "font-size": "15px"
            , "line-height": "24px"
            , "white-space": "pre"
            }
        , ".toast-anchor":
            { "box-sizing": "border-box"
            , "position": "fixed"
            , "display": "flex"
            , "justify-content": "center"
            , "bottom": "32px"
            , "padding": "0 32px"
            , "width": "100%"
            , "z-index": 999999
            , "& > .toast-wrapper":
                { "box-sizing": "border-box"
                , "display": "flex"
                , "transition-property": "opacity, transform"
                , "transition-timing-function": "ease-in-out"
                , transitionDuration
                , "&.empty":
                    { "opacity": 0
                    , "transform": "translateY(30px)"
                    }
                , "&.visible":
                    { "opacity": 1
                    , "transform": "translateY(0)"
                    }
                , "&.expired":
                    { "opacity": 0
                    , "transform": "translateY(30px)"
                    }
                }
            }
        }
    }
