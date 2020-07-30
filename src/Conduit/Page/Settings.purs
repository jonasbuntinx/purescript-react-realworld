module Conduit.Page.Settings where

import Prelude
import Conduit.Component.App as App
import Conduit.Data.Route (Route(..))
import Conduit.Data.Validation as V
import Conduit.Effects.Routing (navigate)
import Conduit.Env (Env)
import Conduit.State.User (logout)
import Control.Comonad (extract)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Lens.Record as LR
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Data.Validation.Semigroup (V, andThen, toEither, unV)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)

data Action
  = UpdateImage String
  | UpdateName String
  | UpdateBio String
  | UpdateEmail String
  | UpdatePassword String
  | Submit
  | Logout

mkSettingsPage :: App.Component Env Unit
mkSettingsPage =
  App.component "SettingsPage" { init, update } \_ store props -> React.do
    pure $ render store props
  where
  init =
    { image: ""
    , name: pure ""
    , bio: ""
    , email: pure ""
    , password: pure ""
    }

  update self = case _ of
    UpdateImage image -> self.setState _ { image = image }
    UpdateName name -> self.setState _ { name = V.Modified name }
    UpdateBio bio -> self.setState _ { bio = bio }
    UpdateEmail email -> self.setState _ { email = V.Modified email }
    UpdatePassword password -> self.setState _ { password = V.Modified password }
    Submit ->
      let
        state = V.setModified self.state
      in
        case toEither (validate state) of
          Left _ -> self.setState (const state)
          Right validated -> pure unit
    Logout -> do
      logout
      navigate Home

  render store props =
    let
      errors = validate store.state # unV identity (const mempty)
    in
      container
        [ R.h1
            { className: "text-xs-center"
            , children:
                [ R.text "Your Settings"
                ]
            }
        , R.form
            { children:
                [ R.fieldset_
                    [ R.fieldset
                        { className: "form-group"
                        , children:
                            [ R.input
                                { className: "form-control"
                                , type: "text"
                                , value: store.state.image
                                , placeholder: "URL of profile picture"
                                , onChange: handler targetValue $ traverse_ $ store.dispatch <<< UpdateImage
                                }
                            ]
                        }
                    , R.fieldset
                        { className: "form-group"
                        , children:
                            [ R.input
                                { className: "form-control"
                                , type: "text"
                                , value: extract store.state.name
                                , placeholder: "Your name"
                                , onChange: handler targetValue $ traverse_ $ store.dispatch <<< UpdateName
                                }
                            , guard (not $ Array.null errors.name)
                                $ R.div
                                    { className: "error-messages"
                                    , children: errors.name <#> \error -> R.div_ [ R.text $ "Name " <> error ]
                                    }
                            ]
                        }
                    , R.fieldset
                        { className: "form-group"
                        , children:
                            [ R.textarea
                                { className: "form-control"
                                , rows: 8
                                , value: store.state.bio
                                , placeholder: "Short bio about you"
                                , onChange: handler targetValue $ traverse_ $ store.dispatch <<< UpdateBio
                                }
                            ]
                        }
                    , R.fieldset
                        { className: "form-group"
                        , children:
                            [ R.input
                                { className: "form-control"
                                , type: "email"
                                , value: extract store.state.email
                                , placeholder: "Email"
                                , onChange: handler targetValue $ traverse_ $ store.dispatch <<< UpdateEmail
                                }
                            , guard (not $ Array.null errors.email)
                                $ R.div
                                    { className: "error-messages"
                                    , children: errors.email <#> \error -> R.div_ [ R.text $ "Email " <> error ]
                                    }
                            ]
                        }
                    , R.fieldset
                        { className: "form-group"
                        , children:
                            [ R.input
                                { className: "form-control"
                                , type: "password"
                                , value: extract store.state.password
                                , placeholder: "Password"
                                , onChange: handler targetValue $ traverse_ $ store.dispatch <<< UpdatePassword
                                }
                            , guard (not $ Array.null errors.password)
                                $ R.div
                                    { className: "error-messages"
                                    , children: errors.password <#> \error -> R.div_ [ R.text $ "Password " <> error ]
                                    }
                            ]
                        }
                    , R.button
                        { className: "btn btn-primary pull-xs-right"
                        , type: "button"
                        , onClick: handler_ $ store.dispatch Submit
                        , children: [ R.text "Update settings" ]
                        }
                    ]
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

-- | Validation
type ValidationValues r
  = { image :: String
    , name :: V.Validated String
    , bio :: String
    , email :: V.Validated String
    , password :: V.Validated String
    | r
    }

type ValidationErrors
  = { name :: Array String
    , email :: Array String
    , password :: Array String
    }

type ValidatedValues
  = { image :: String
    , name :: String
    , bio :: String
    , email :: String
    , password :: String
    }

validate :: forall r. ValidationValues r -> V ValidationErrors ValidatedValues
validate values = ado
  name <- V.validate validateName (V.toRecord (LR.prop (SProxy :: _ "name"))) values.name
  email <- V.validate validateEmail (V.toRecord (LR.prop (SProxy :: _ "email"))) values.email
  password <- V.validate validatePassword (V.toRecord (LR.prop (SProxy :: _ "password"))) values.password
  in { image: values.image, name, bio: values.bio, email, password }
  where
  validateName name = do
    V.validateNonEmpty name
      `andThen`
        ( V.validateMinimumLength 3 <> V.validateMaximunLength 20
        )

  validateEmail email = do
    V.validateNonEmpty email `andThen` V.validateEmailFormat

  validatePassword password = do
    V.validateNonEmpty password
      `andThen`
        ( V.validateMinimumLength 3 <> V.validateMaximunLength 20
        )
