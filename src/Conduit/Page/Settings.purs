module Conduit.Page.Settings (makeSettingsPage) where

import Prelude
import Conduit.Capability.Api (updateUser)
import Conduit.Capability.Auth (logout, updateProfile)
import Conduit.Capability.Routing (navigate, redirect)
import Conduit.Component.Page as Page
import Conduit.Component.ResponseErrors (responseErrors)
import Conduit.Data.Avatar as Avatar
import Conduit.Data.Profile (UserProfile)
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username as Username
import Conduit.Form.Validated as V
import Conduit.Form.Validator as F
import Conduit.Hook.Auth (useProfile)
import Control.Comonad (extract)
import Control.Monad.State (modify_)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Lens.Record as LR
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Data.Validation.Semigroup (andThen, toEither, unV)
import Network.RemoteData as RemoteData
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks as React
import Record as Record

data Action
  = Initialize UserProfile
  | UpdateImage String
  | UpdateUsername String
  | UpdateBio String
  | UpdateEmail String
  | UpdatePassword String
  | Submit
  | Logout

makeSettingsPage :: Page.Component Unit
makeSettingsPage =
  Page.component "SettingsPage" { initialState, update } \store -> React.do
    profile <- useProfile store.env
    React.useEffect profile do
      for_ profile $ store.send <<< Initialize
      mempty
    pure $ render store
  where
  initialState =
    { profile: Nothing
    , image: Nothing
    , username: pure ""
    , bio: Nothing
    , email: pure ""
    , password: pure ""
    , submitResponse: RemoteData.NotAsked
    }

  update self = case _ of
    Initialize profile ->
      modify_
        _
          { profile = Just profile
          , image = Avatar.toString <$> profile.image
          , username = pure $ Username.toString profile.username
          , bio = profile.bio
          , email = pure profile.email
          }
    UpdateImage image -> modify_ _ { image = Just image }
    UpdateUsername username -> modify_ _ { username = V.Modified username }
    UpdateBio bio -> modify_ _ { bio = Just bio }
    UpdateEmail email -> modify_ _ { email = V.Modified email }
    UpdatePassword password -> modify_ _ { password = V.Modified password }
    Submit -> do
      let
        state = V.setModified self.state
      case toEither (validate state) of
        Left _ -> modify_ (const state)
        Right validated -> do
          modify_ _ { submitResponse = RemoteData.Loading }
          response <- updateUser validated
          case response of
            Right user -> do
              modify_ _ { submitResponse = RemoteData.Success unit }
              updateProfile $ Record.delete (SProxy :: _ "token") user
              navigate Home
            Left err -> modify_ _ { submitResponse = RemoteData.Failure err }
    Logout -> logout *> redirect Home

  validate values = ado
    username <- values.username # V.validated (LR.prop (SProxy :: _ "username")) \username -> F.nonEmpty username `andThen` F.validUsername
    email <- values.email # V.validated (LR.prop (SProxy :: _ "email")) \email -> F.nonEmpty email `andThen` F.validEmail
    password <- values.password # V.validated (LR.prop (SProxy :: _ "password")) \password -> F.nonEmpty password `andThen` (F.minimumLength 3 *> F.maximunLength 20)
    in { image: Avatar.fromString <$> values.image, username, bio: values.bio, email, password }

  render { props, state, send } =
    let
      errors = validate state # unV identity (const mempty) :: { username :: _, email :: _, password :: _ }
    in
      guard (isJust state.profile) container
        [ R.h1
            { className: "text-xs-center"
            , children: [ R.text "Your Settings" ]
            }
        , responseErrors state.submitResponse
        , R.form
            { children:
                [ R.fieldset_
                    [ R.fieldset
                        { className: "form-group"
                        , children:
                            [ R.input
                                { className: "form-control"
                                , type: "text"
                                , value: fromMaybe "" state.image
                                , placeholder: "URL of profile picture"
                                , onChange: handler targetValue $ traverse_ $ send <<< UpdateImage
                                }
                            ]
                        }
                    , R.fieldset
                        { className: "form-group"
                        , children:
                            [ R.input
                                { className: "form-control"
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
                            [ R.textarea
                                { className: "form-control"
                                , rows: 8
                                , value: fromMaybe "" state.bio
                                , placeholder: "Short bio about you"
                                , onChange: handler targetValue $ traverse_ $ send <<< UpdateBio
                                }
                            ]
                        }
                    , R.fieldset
                        { className: "form-group"
                        , children:
                            [ R.input
                                { className: "form-control"
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
                                { className: "form-control"
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
                        { className: "btn btn-primary pull-xs-right"
                        , type: "button"
                        , onClick: handler_ $ send Submit
                        , children: [ R.text "Update settings" ]
                        }
                    ]
                ]
            }
        , R.hr {}
        , R.button
            { className: "btn btn-outline-danger"
            , type: "button"
            , onClick: handler_ $ send Logout
            , children: [ R.text "Log out" ]
            }
        ]
    where
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
                                { className: "col-md-6 offset-md-3 col-xs-12"
                                , children
                                }
                            ]
                        }
                    ]
                }
            ]
        }
