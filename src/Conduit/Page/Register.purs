module Conduit.Page.Register (makeRegisterPage) where

import Prelude
import Conduit.Capability.Api (registerUser)
import Conduit.Capability.Auth (login)
import Conduit.Capability.Routing (redirect)
import Conduit.Component.Link as Link
import Conduit.Component.ResponseErrors (responseErrors)
import Conduit.Component.Store as Store
import Conduit.Data.Route (Route(..))
import Conduit.Form.Validated as V
import Conduit.Form.Validator as F
import Control.Comonad (extract)
import Control.Monad.State (modify_)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Lens.Record as LR
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Data.Validation.Semigroup (andThen, toEither, unV)
import Network.RemoteData as RemoteData
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import Record as Record

data Action
  = UpdateUsername String
  | UpdateEmail String
  | UpdatePassword String
  | Submit

makeRegisterPage :: Store.Component Unit
makeRegisterPage =
  Store.component "RegisterPage" { initialState, update } \store -> React.do
    pure $ render store
  where
  initialState =
    { username: pure ""
    , email: pure ""
    , password: pure ""
    , submitResponse: RemoteData.NotAsked
    }

  update self = case _ of
    UpdateUsername username -> modify_ _ { username = V.Modified username }
    UpdateEmail email -> modify_ _ { email = V.Modified email }
    UpdatePassword password -> modify_ _ { password = V.Modified password }
    Submit -> do
      let
        state = V.setModified self.state
      case toEither (validate state) of
        Left _ -> modify_ (const state)
        Right validated -> do
          modify_ _ { submitResponse = RemoteData.Loading }
          bind (registerUser validated) case _ of
            Right user -> do
              modify_ _ { submitResponse = RemoteData.Success unit }
              login user.token $ Record.delete (SProxy :: _ "token") user
              redirect Home
            Left err -> modify_ _ { submitResponse = RemoteData.Failure err }

  validate values = ado
    username <- values.username # V.validated (LR.prop (SProxy :: _ "username")) \username -> F.nonEmpty username `andThen` F.validUsername
    email <- values.email # V.validated (LR.prop (SProxy :: _ "email")) \email -> F.nonEmpty email `andThen` F.validEmail
    password <- values.password # V.validated (LR.prop (SProxy :: _ "password")) \password -> F.nonEmpty password `andThen` (F.minimumLength 3 *> F.maximunLength 20)
    in { username, email, password }

  render { env, props, state, send } =
    let
      errors = validate state # unV identity (const mempty) :: { username :: _, email :: _, password :: _ }
    in
      container
        [ R.h1
            { className: "text-xs-center"
            , children: [ R.text "Sign up" ]
            }
        , R.p
            { className: "text-xs-center"
            , children:
                [ Link.link
                    { className: ""
                    , route: Login
                    , onClick: env.router.navigate
                    , children: [ R.text "Already have an account?" ]
                    }
                ]
            }
        , responseErrors state.submitResponse
        , R.form
            { children:
                [ R.fieldset_
                    [ R.fieldset
                        { className: "form-group"
                        , children:
                            [ R.input
                                { className: "form-control form-control-lg"
                                , type: "text"
                                , value: extract state.username
                                , placeholder: "Your name"
                                , onChange: handler targetValue $ traverse_ $ send <<< UpdateUsername
                                }
                            , guard (not $ Array.null errors.username) R.div
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
                                , autoComplete: "UserName"
                                , type: "email"
                                , value: extract state.email
                                , placeholder: "Email"
                                , onChange: handler targetValue $ traverse_ $ send <<< UpdateEmail
                                }
                            , guard (not $ Array.null errors.email) R.div
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
                                , autoComplete: "Password"
                                , type: "password"
                                , value: extract state.password
                                , placeholder: "Password"
                                , onChange: handler targetValue $ traverse_ $ send <<< UpdatePassword
                                }
                            , guard (not $ Array.null errors.password) R.div
                                { className: "error-messages"
                                , children: errors.password <#> \error -> R.div_ [ R.text $ "Password " <> error ]
                                }
                            ]
                        }
                    , R.button
                        { className: "btn btn-lg btn-primary pull-xs-right"
                        , type: "button"
                        , onClick: handler_ $ send Submit
                        , children: [ R.text "Sign up" ]
                        }
                    ]
                ]
            }
        ]
    where
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
                                { className: "col-md-6 offset-md-3 col-xs-12"
                                , children
                                }
                            ]
                        }
                    ]
                }
            ]
        }
