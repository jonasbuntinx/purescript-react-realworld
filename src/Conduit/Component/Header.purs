module Conduit.Component.Header where

import Prelude
import Conduit.Data.Avatar as Avatar
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username as Username
import Conduit.Effects.Routing (navigate)
import Conduit.Env.Auth (Auth)
import Data.Maybe (Maybe, isJust, isNothing, maybe)
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
                        , guard (isNothing auth) navItem Login [ R.text "Sign in" ]
                        , guard (isNothing auth) navItem Register [ R.text "Sign up" ]
                        , guard (isJust auth) navItem CreateArticle
                            [ R.i { className: "ion-compose", children: [] }
                            , R.text " New Article"
                            ]
                        , guard (isJust auth) navItem Settings
                            [ R.i { className: "ion-gear-a", children: [] }
                            , R.text " Settings"
                            ]
                        , auth
                            # maybe React.empty \{ username, profile } ->
                                navItem (Profile username)
                                  [ R.img
                                      { className: "user-pic"
                                      , src: Avatar.toString $ maybe Avatar.blank (Avatar.withDefault <<< _.image) profile
                                      }
                                  , R.text $ " " <> Username.toString username
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
