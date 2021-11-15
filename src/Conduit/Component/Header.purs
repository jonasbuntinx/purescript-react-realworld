module Conduit.Component.Header where

import Prelude
import Conduit.Component.Link as Link
import Conduit.Data.Auth (Auth)
import Conduit.Data.Avatar as Avatar
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username as Username
import Data.Maybe (Maybe, isJust, isNothing, maybe)
import Data.Monoid (guard)
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Hooks as React

type Props
  =
  { auth :: Maybe Auth
  , currentRoute :: Route
  , onNavigate :: Route -> Effect Unit
  }

header :: Props -> React.JSX
header { auth, currentRoute, onNavigate } =
  R.nav
    { className: "navbar navbar-light"
    , children:
        [ R.div
            { className: "container"
            , children:
                [ Link.link
                    { className: "navbar-brand"
                    , route: Home
                    , onClick: onNavigate
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
                            # maybe React.empty \{ username, user } ->
                                navItem (Profile $ maybe username _.username user)
                                  [ R.img
                                      { className: "user-pic"
                                      , src: Avatar.toString $ maybe Avatar.blank (Avatar.withDefault <<< _.image) user
                                      }
                                  , R.text $ " " <> Username.toString (maybe username _.username user)
                                  ]
                        ]
                    }
                ]
            }
        ]
    }
  where
  navItem route children =
    R.li
      { className: "nav-item"
      , children:
          [ Link.link
              { className: "nav-link" <> guard (currentRoute == route) " active"
              , route
              , onClick: onNavigate
              , children
              }
          ]
      }
