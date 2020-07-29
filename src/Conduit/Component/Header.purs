module Conduit.Component.Header where

import Prelude
import Conduit.Data.Auth (Auth)
import Conduit.Data.Route (Route(..))
import Conduit.Effects.Routing (navigate)
import Data.Maybe (Maybe, isJust, isNothing)
import Data.Monoid (guard)
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import React.Basic.Hooks as React

header :: Maybe Auth -> Route -> React.JSX
header auth route =
  R.nav
    { className: "navbar navbar-light"
    , children:
        [ R.div
            { className: "container"
            , children:
                [ R.a
                    { className: "navbar-brand"
                    , href: "#"
                    , onClick: handler preventDefault $ const $ navigate Home
                    , children: [ R.text "conduit" ]
                    }
                , R.ul
                    { className: "nav navbar-nav pull-xs-right"
                    , children:
                        [ navItem Home [ R.text "Home" ]
                        , guard (isJust auth)
                            $ React.fragment
                                [ navItem Editor
                                    [ R.i { className: "ion-compose", children: [] }
                                    , R.text " New Article"
                                    ]
                                , navItem Settings
                                    [ R.i { className: "ion-gear-a", children: [] }
                                    , R.text " Settings"
                                    ]
                                , navItem (Profile "")
                                    [ R.img
                                        { className: "user-pic"
                                        , src: ""
                                        }
                                    , R.text " Profile"
                                    ]
                                ]
                        , guard (isNothing auth)
                            React.fragment
                            [ navItem Login [ R.text "Sign in" ]
                            , navItem Register [ R.text "Sign up" ]
                            ]
                        ]
                    }
                ]
            }
        ]
    }
  where
  navItem r children =
    R.li
      { className: "nav-item"
      , children:
          [ R.a
              { className: "nav-link" <> guard (route == r) " active"
              , href: "#"
              , onClick: handler preventDefault $ const $ navigate r
              , children
              }
          ]
      }
