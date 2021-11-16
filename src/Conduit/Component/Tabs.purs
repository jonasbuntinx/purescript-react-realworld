module Conduit.Component.Tabs where

import Prelude
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Maybe (Maybe, maybe)
import Data.Monoid (guard)
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import React.Basic.Hooks as React

type Props a =
  { className :: String
  , selectedTab :: Maybe a
  , tabs ::
      Array
        { id :: a
        , label :: React.JSX
        , disabled :: Boolean
        , content :: React.JSX
        }
  , onChange :: a -> Effect Unit
  }

tabs :: forall a. Eq a => Props a -> React.JSX
tabs props =
  React.fragment
    [ R.div
        { className: props.className
        , children:
            [ R.ul
                { className: "nav nav-pills outline-active"
                , children:
                    props.tabs
                      <#> \{ id, label, disabled } ->
                        guard (not disabled) R.li
                          { className: "nav-item"
                          , children:
                              [ R.a
                                  { className: "nav-link" <> guard (isActive id) " active"
                                  , href: "#"
                                  , onClick: handler preventDefault $ const $ props.onChange id
                                  , children: [ label ]
                                  }
                              ]
                          }
                }
            ]
        }
    , foldMap _.content activeTab
    ]
  where
  isActive id = maybe false (eq id <<< _.id) activeTab

  activeTab = selectedTab <|> firstTab

  selectedTab = props.selectedTab >>= \seletedTab -> Array.find (\{ id, disabled } -> (not disabled) && (eq seletedTab id)) props.tabs

  firstTab = Array.head props.tabs
