module Conduit.Page.Login where

import Prelude
import Apiary.Route (Route(..)) as Apiary
import Apiary.Types (none) as Apiary
import Conduit.Api.User (Login)
import Conduit.Api.Utils as Utils
import Conduit.Component.App as App
import Conduit.Data.Route (Route(..))
import Conduit.Data.Validation (Validated(..), invalid, isValidEmail, modified, setModified)
import Conduit.Effects.Routing (navigate)
import Conduit.Env (Env)
import Conduit.State.Auth (login)
import Control.Comonad (extract)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Lens.Record as LR
import Data.Monoid (guard)
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Validation.Semigroup (V, andThen, toEither, unV)
import Data.Variant as Variant
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events (handler, handler_)

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
    UpdateEmail email -> self.setState _ { email = Modified email }
    UpdatePassword password -> self.setState _ { password = Modified password }
    Submit ->
      let
        state = setModified self.state
      in
        case toEither (validate state) of
          Left _ -> self.setState (const state)
          Right validated -> do
            res <- Utils.makeRequest (Apiary.Route :: Login) Apiary.none Apiary.none { user: validated }
            case res of
              Left err -> self.setState _ { error = true }
              Right success -> do
                success
                  # Variant.match
                      { ok:
                          \auth -> do
                            login auth.user.token
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
                [ R.div
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
                                , children: errors.email <#> \error -> R.div_ [ R.text error ]
                                }
                        ]
                    }
                , R.div
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
                                , children: errors.password <#> \error -> R.div_ [ R.text error ]
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
  = { email :: Validated String
    , password :: Validated String
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
  email <- modified validateEmail values.email
  password <- modified validatePassword values.password
  in { email, password }
  where
  validateEmail email = do
    validateNonEmpty email `andThen` (validateMinimumLength 3 *> validateFormat)
    where
    throwError err = invalid (LR.prop (SProxy :: _ "email")) (Array.singleton err)

    validateNonEmpty input
      | String.null input = throwError "Email is required"
      | otherwise = pure input

    validateMinimumLength validLength input
      | String.length input <= validLength = throwError "Email is too short"
      | otherwise = pure input

    validateFormat input
      | not $ isValidEmail input = throwError "Email is invalid"
      | otherwise = pure input

  validatePassword password = do
    validateNonEmpty password `andThen` (validateMinimumLength 2 *> validateMaximunLength 20)
    where
    throwError err = invalid (LR.prop (SProxy :: _ "password")) (Array.singleton err)

    validateNonEmpty input
      | String.null input = throwError "Password is required"
      | otherwise = pure input

    validateMinimumLength validLength input
      | String.length input <= validLength = throwError "Password is too short"
      | otherwise = pure input

    validateMaximunLength validLength input
      | String.length input > validLength = throwError "Password is too long"
      | otherwise = pure input
