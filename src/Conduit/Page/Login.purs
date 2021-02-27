module Conduit.Page.Login (mkLoginPage) where

import Prelude
import Conduit.Capability.Halo (class MonadHalo, JSX, component_)
import Conduit.Capability.Resource.User (class UserRepository, loginUser)
import Conduit.Capability.Routing (class MonadRouting, navigate, redirect)
import Conduit.Component.Link as Link
import Conduit.Component.ResponseErrors (responseErrors)
import Conduit.Data.Route (Route(..))
import Conduit.Form.Validated as V
import Conduit.Form.Validator as F
import Control.Comonad (extract)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Lens.Record as LR
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Data.Validation.Semigroup (andThen, toEither, unV)
import Network.RemoteData as RemoteData
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Halo as Halo

data Action
  = Navigate Route
  | UpdateEmail String
  | UpdatePassword String
  | Submit

mkLoginPage ::
  forall m.
  MonadRouting m =>
  UserRepository m =>
  MonadHalo m =>
  m JSX
mkLoginPage = component_ "LoginPage" { initialState, eval, render }
  where
  initialState =
    { email: pure ""
    , password: pure ""
    , submitResponse: RemoteData.NotAsked
    }

  eval =
    Halo.mkEval
      _
        { onAction = handleAction
        }

  handleAction = case _ of
    Navigate route -> navigate route
    UpdateEmail email -> Halo.modify_ _ { email = V.Modified email }
    UpdatePassword password -> Halo.modify_ _ { password = V.Modified password }
    Submit -> do
      state <- V.setModified <$> Halo.get
      case toEither (validate state) of
        Left _ -> Halo.modify_ (const state)
        Right validated -> do
          Halo.modify_ _ { submitResponse = RemoteData.Loading }
          response <- loginUser validated
          Halo.modify_ _ { submitResponse = RemoteData.fromEither response }
          for_ response \_ -> redirect Home

  validate values = ado
    email <- values.email # V.validated (LR.prop (SProxy :: _ "email")) \email -> F.nonEmpty email `andThen` F.validEmail
    password <- values.password # V.validated (LR.prop (SProxy :: _ "password")) \password -> F.nonEmpty password `andThen` (F.minimumLength 3 *> F.maximunLength 20)
    in { email, password }

  render { state, send } =
    let
      errors = validate state # unV identity (const mempty) :: { email :: _, password :: _ }
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
                    , onClick: send <<< Navigate
                    , children: [ R.text "Need an account?" ]
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
                            , guard (not $ Array.null errors.email) R.div
                                { className: "error-messages"
                                , children: errors.password <#> \error -> R.div_ [ R.text $ "Password " <> error ]
                                }
                            ]
                        }
                    , R.button
                        { className: "btn btn-lg btn-primary pull-xs-right"
                        , type: "button"
                        , onClick: handler_ $ send Submit
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
