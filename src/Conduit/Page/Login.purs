module Conduit.Page.Login where

import Prelude
import Apiary.Route (Route(..)) as Apiary
import Apiary.Types (none) as Apiary
import Conduit.Api.Request as Request
import Conduit.Api.User (Login)
import Conduit.Component.App as App
import Conduit.Data.Route (Route(..))
import Conduit.Data.Validation as V
import Conduit.Effects.Routing (navigate)
import Conduit.Env (Env)
import Conduit.State.User (login)
import Control.Comonad (extract)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Lens.Record as LR
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Data.Validation.Semigroup (V, andThen, toEither, unV)
import Data.Variant as Variant
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events (handler, handler_)
import Record as Record

data Action
  = UpdateEmail String
  | UpdatePassword String
  | Submit

mkLoginPage :: App.Component Env Unit
mkLoginPage =
  App.component "LoginPage" { init, update } \_ store props -> React.do
    pure $ render store props
  where
  init =
    { email: pure ""
    , password: pure ""
    , error: false
    }

  update self = case _ of
    UpdateEmail email -> self.setState _ { email = V.Modified email }
    UpdatePassword password -> self.setState _ { password = V.Modified password }
    Submit ->
      let
        state = V.setModified self.state
      in
        case toEither (validate state) of
          Left _ -> self.setState (const state)
          Right validated -> do
            res <- Request.makeRequest (Apiary.Route :: Login) Apiary.none Apiary.none { user: validated }
            case res of
              Left err -> self.setState _ { error = true }
              Right success -> do
                success
                  # Variant.match
                      { ok:
                          \{ user } -> do
                            login user.token $ Record.delete (SProxy :: _ "token") user
                            navigate Home
                      }

  render store props =
    let
      errors = validate store.state # unV identity (const mempty)
    in
      container
        [ R.h1
            { className: "text-xs-center"
            , children:
                [ R.text "Sign in"
                ]
            }
        , R.p
            { className: "text-xs-center"
            , children:
                [ R.a
                    { href: "#"
                    , onClick: handler preventDefault $ const $ navigate Register
                    , children: [ R.text "Need an account?" ]
                    }
                ]
            }
        , guard store.state.error
            $ R.div
                { className: "error-messages"
                , children:
                    [ R.div_
                        [ R.text "Email or password is invalid" ]
                    ]
                }
        , R.form
            { children:
                [ R.fieldset_
                    [ R.fieldset
                        { className: "form-group"
                        , children:
                            [ R.input
                                { className: "form-control form-control-lg"
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
                                { className: "form-control form-control-lg"
                                , type: "password"
                                , value: extract store.state.password
                                , placeholder: "Password"
                                , onChange: handler targetValue $ traverse_ $ store.dispatch <<< UpdatePassword
                                }
                            , guard (not $ Array.null errors.email)
                                $ R.div
                                    { className: "error-messages"
                                    , children: errors.password <#> \error -> R.div_ [ R.text $ "Password " <> error ]
                                    }
                            ]
                        }
                    , R.button
                        { className: "btn btn-lg btn-primary pull-xs-right"
                        , type: "button"
                        , onClick: handler_ $ store.dispatch Submit
                        , children: [ R.text "Sign in" ]
                        }
                    ]
                ]
            }
        ]

  container children =
    R.div
      { className: "auth-page"
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
  = { email :: V.Validated String
    , password :: V.Validated String
    | r
    }

type ValidationErrors
  = { email :: Array String
    , password :: Array String
    }

type ValidatedValues
  = { email :: String
    , password :: String
    }

validate :: forall r. ValidationValues r -> V ValidationErrors ValidatedValues
validate values = ado
  email <-
    values.email
      # V.validate (V.toRecord (LR.prop (SProxy :: _ "email"))) \email -> do
          V.validateNonEmpty email `andThen` V.validateEmailFormat
  password <-
    values.password
      # V.validate (V.toRecord (LR.prop (SProxy :: _ "password"))) \password -> do
          V.validateNonEmpty password
            `andThen`
              ( V.validateMinimumLength 3 *> V.validateMaximunLength 20
              )
  in { email, password }
