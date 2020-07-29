module Conduit.Page.Settings where

import Prelude
import Conduit.Component.App as App
import Conduit.Data.Route (Route(..))
import Conduit.Effects.Routing (navigate)
import Conduit.Env (Env)
import Conduit.State.User (logout)
import React.Basic.DOM as R
import React.Basic.Events (handler_)

data Action
  = Logout

mkSettingsPage :: App.Component Env Unit
mkSettingsPage =
  App.component "SettingsPage" { init, update } \_ store props -> React.do
    pure $ render store props
  where
  init = {}

  update self = case _ of
    Logout -> do
      logout
      navigate Home

  render store props =
    container
      [ R.h1
          { className: "text-xs-center"
          , children:
              [ R.text "Your Settings"
              ]
          }
      , R.hr {}
      , R.button
          { className: "btn btn-outline-danger"
          , type: "button"
          , onClick: handler_ $ store.dispatch Logout
          , children: [ R.text "Log out" ]
          }
      ]

  container children =
    R.div
      { className: "settings-page"
      , children:
          [ R.div
              { className: "container page"
              , children:
                  [ R.div
                      { className: "row"
                      , children:
                          [ R.div
                              { className: "col-md-6 offset-md-3 col-xs12"
                              , children
                              }
                          ]
                      }
                  ]
              }
          ]
      }
