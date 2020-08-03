module Conduit.Component.Tabs where

import Prelude
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Maybe (Maybe, maybe)
import Data.Monoid (guard)
import Data.Newtype (class Newtype)
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import React.Basic.Hooks as React

newtype TabId
  = TabId String

derive instance newtypeTabId :: Newtype TabId _

derive instance eqTabId :: Eq TabId

type Props
  = { className :: String
    , selectedTab :: Maybe TabId
    , tabs ::
        Array
          { id :: TabId
          , label :: String
          , content :: React.JSX
          }
    , onChange :: TabId -> Effect Unit
    }

tabs :: Props -> React.JSX
tabs props =
  React.fragment
    [ R.div
        { className: props.className
        , children:
            [ R.ul
                { className: "nav nav-pills outline-active"
                , children:
                    props.tabs
                      <#> \{ id, label } ->
                          R.li
                            { className: "nav-item"
                            , children:
                                [ R.a
                                    { className: "nav-link" <> guard (isActive id) " active"
                                    , href: "#"
                                    , onClick: handler preventDefault $ const $ props.onChange id
                                    , children: [ R.text label ]
                                    }
                                ]
                            }
                }
            ]
        }
    , Array.foldMap _.content activeTab
    ]
  where
  isActive id = maybe false (eq id <<< _.id) activeTab

  activeTab = selectedTab <|> firstTab

  selectedTab = props.selectedTab >>= \id -> Array.find (eq id <<< _.id) props.tabs

  firstTab = Array.head props.tabs
