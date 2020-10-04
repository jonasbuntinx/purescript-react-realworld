module Conduit.Page.Login (makeLoginPage) where

import Prelude
import Conduit.Capability.Api (loginUser)
import Conduit.Capability.Auth (login)
import Conduit.Capability.Routing (redirect)
import Conduit.Component.Env as Env
import Conduit.Component.Link as Link
import Conduit.Component.ResponseErrors (responseErrors)
import Conduit.Data.Route (Route(..))
import Conduit.Form.Validated as V
import Conduit.Form.Validator as F
import Control.Comonad (extract)
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
  = UpdateEmail String
  | UpdatePassword String
  | Submit

makeLoginPage :: Env.Component Unit
makeLoginPage =
  Env.component "LoginPage" { init, update } \env store props -> React.do
    pure $ render env store props
  where
  init =
    { email: pure ""
    , password: pure ""
    , submitResponse: RemoteData.NotAsked
    }

  update self = case _ of
    UpdateEmail email -> self.setState _ { email = V.Modified email }
    UpdatePassword password -> self.setState _ { password = V.Modified password }
    Submit -> do
      let
        state = V.setModified self.state
      case toEither (validate state) of
        Left _ -> self.setState (const state)
        Right validated -> do
          self.setState _ { submitResponse = RemoteData.Loading }
          bind (loginUser validated) case _ of
            Right user -> do
              self.setState _ { submitResponse = RemoteData.Success unit }
              login user.token $ Record.delete (SProxy :: _ "token") user
              redirect Home
            Left err -> self.setState _ { submitResponse = RemoteData.Failure err }

  validate values = ado
    email <- values.email # V.validated (LR.prop (SProxy :: _ "email")) \email -> F.nonEmpty email `andThen` F.validEmail
    password <- values.password # V.validated (LR.prop (SProxy :: _ "password")) \password -> F.nonEmpty password `andThen` (F.minimumLength 3 *> F.maximunLength 20)
    in { email, password }

  render env store props =
    let
      errors = validate store.state # unV identity (const mempty) :: { email :: _, password :: _ }
    in
      container
        [ R.h1
            { className: "text-xs-center"
            , children: [ R.text "Sign in" ]
            }
        , R.p
            { className: "text-xs-center"
            , children:
                [ Link.link
                    { className: ""
                    , route: Register
                    , onClick: env.router.navigate
                    , children: [ R.text "Need an account?" ]
                    }
                ]
            }
        , responseErrors store.state.submitResponse
        , R.form
            { children:
                [ R.fieldset_
                    [ R.fieldset
                        { className: "form-group"
                        , children:
                            [ R.input
                                { className: "form-control form-control-lg"
                                , autoComplete: "UserName"
                                , type: "email"
                                , value: extract store.state.email
                                , placeholder: "Email"
                                , onChange: handler targetValue $ traverse_ $ store.dispatch <<< UpdateEmail
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
                                , value: extract store.state.password
                                , placeholder: "Password"
                                , onChange: handler targetValue $ traverse_ $ store.dispatch <<< UpdatePassword
                                }
                            , guard (not $ Array.null errors.email) R.div
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
