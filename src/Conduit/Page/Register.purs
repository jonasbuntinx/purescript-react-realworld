module Conduit.Page.Register where

import Prelude
import Apiary.Route (Route(..)) as Apiary
import Apiary.Types (none) as Apiary
import Conduit.Api.User (Register)
import Conduit.Api.Utils as Utils
import Conduit.Component.App as App
import Conduit.Component.Link as Link
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username (Username)
import Conduit.Data.Validation as V
import Conduit.Effects.Routing (redirect)
import Conduit.Env (Env)
import Conduit.Env.Auth (login)
import Conduit.Form.Validation as F
import Control.Comonad (extract)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Lens.Record as LR
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Data.Validation.Semigroup (V, andThen, toEither, unV)
import Data.Variant as Variant
import Foreign.Object as Object
import Network.RemoteData as RemoteData
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks as React
import Record as Record

data Action
  = UpdateUsername String
  | UpdateEmail String
  | UpdatePassword String
  | Submit

mkRegisterPage :: App.Component Env Unit
mkRegisterPage =
  App.component "RegisterPage" { init, update } \_ store props -> React.do
    pure $ render store props
  where
  init =
    { username: pure ""
    , email: pure ""
    , password: pure ""
    , submitResponse: RemoteData.NotAsked
    }

  update self = case _ of
    UpdateUsername username -> self.setState _ { username = V.Modified username }
    UpdateEmail email -> self.setState _ { email = V.Modified email }
    UpdatePassword password -> self.setState _ { password = V.Modified password }
    Submit ->
      let
        state = V.setModified self.state
      in
        case toEither (validate state) of
          Left _ -> self.setState (const state)
          Right validated -> do
            self.setState _ { submitResponse = RemoteData.Loading }
            res <- Utils.makeRequest (Apiary.Route :: Register) Apiary.none Apiary.none { user: validated }
            case res of
              Left _ -> do
                self.setState _ { submitResponse = RemoteData.Failure (Object.singleton "unknown error:" [ "request failed" ]) }
              Right response ->
                response
                  # Variant.match
                      { ok:
                          \{ user } -> do
                            self.setState _ { submitResponse = RemoteData.Success unit }
                            login user.token $ Record.delete (SProxy :: _ "token") user
                            redirect Home
                      , unprocessableEntity:
                          \{ errors } ->
                            self.setState _ { submitResponse = RemoteData.Failure errors }
                      }

  render store props =
    let
      errors = validate store.state # unV identity (const mempty)
    in
      container
        [ R.h1
            { className: "text-xs-center"
            , children:
                [ R.text "Sign up"
                ]
            }
        , R.p
            { className: "text-xs-center"
            , children:
                [ Link.link
                    { className: ""
                    , route: Login
                    , children: [ R.text "Already have an account?" ]
                    }
                ]
            }
        , case store.state.submitResponse of
            RemoteData.Failure submissionErrors ->
              R.ul
                { className: "error-messages"
                , children:
                    submissionErrors
                      # Object.foldMap \key value -> value <#> \error -> R.li_ [ R.text $ key <> " " <> error ]
                }
            _ -> React.empty
        , R.form
            { children:
                [ R.fieldset_
                    [ R.fieldset
                        { className: "form-group"
                        , children:
                            [ R.input
                                { className: "form-control form-control-lg"
                                , type: "text"
                                , value: extract store.state.username
                                , placeholder: "Your name"
                                , onChange: handler targetValue $ traverse_ $ store.dispatch <<< UpdateUsername
                                }
                            , guard (not $ Array.null errors.username)
                                $ R.div
                                    { className: "error-messages"
                                    , children: errors.username <#> \error -> R.div_ [ R.text $ "Name " <> error ]
                                    }
                            ]
                        }
                    , R.fieldset
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
                            , guard (not $ Array.null errors.password)
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
                        , children: [ R.text "Sign up" ]
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
  = { username :: V.Validated String
    , email :: V.Validated String
    , password :: V.Validated String
    | r
    }

type ValidationErrors
  = { username :: Array String
    , email :: Array String
    , password :: Array String
    }

type ValidatedValues
  = { username :: Username
    , email :: String
    , password :: String
    }

validate :: forall r. ValidationValues r -> V ValidationErrors ValidatedValues
validate values = ado
  username <-
    values.username
      # V.validate (LR.prop (SProxy :: _ "username")) \username -> do
          F.validateNonEmpty username
            `andThen`
              F.validateUsernameFormat
  email <-
    values.email
      # V.validate (LR.prop (SProxy :: _ "email")) \email -> do
          F.validateNonEmpty email `andThen` F.validateEmailFormat
  password <-
    values.password
      # V.validate (LR.prop (SProxy :: _ "password")) \password -> do
          F.validateNonEmpty password
            `andThen`
              ( F.validateMinimumLength 3 *> F.validateMaximunLength 20
              )
  in { username, email, password }
