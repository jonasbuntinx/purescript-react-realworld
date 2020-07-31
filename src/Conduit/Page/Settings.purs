module Conduit.Page.Settings where

import Prelude
import Apiary.Route (Route(..)) as Apiary
import Apiary.Types (none) as Apiary
import Conduit.Api.Request as Request
import Conduit.Api.User (UpdateUser)
import Conduit.Component.App as App
import Conduit.Data.Profile (Profile)
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username (Username)
import Conduit.Data.Username as Username
import Conduit.Data.Validation as V
import Conduit.Effects.Routing (navigate, redirect)
import Conduit.Env (Env)
import Conduit.Env.User (logout, updateProfile)
import Conduit.Hook.User (useProfile)
import Control.Comonad (extract)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Lens.Record as LR
import Data.Maybe (Maybe(..), fromMaybe, isJust)
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
  = Initialize Profile
  | UpdateImage String
  | UpdateUsername String
  | UpdateBio String
  | UpdateEmail String
  | UpdatePassword String
  | Submit
  | Logout

mkSettingsPage :: App.Component Env Unit
mkSettingsPage =
  App.component "SettingsPage" { init, update } \env store props -> React.do
    profile <- useProfile env
    React.useEffect profile do
      for_ profile $ store.dispatch <<< Initialize
      mempty
    pure $ render store props
  where
  init =
    { profile: Nothing
    , image: Nothing
    , username: pure ""
    , bio: Nothing
    , email: pure ""
    , password: pure ""
    , submitResponse: RemoteData.NotAsked
    }

  update self = case _ of
    Initialize profile -> do
      self.setState
        _
          { profile = Just profile
          , image = profile.image
          , username = pure $ Username.toString profile.username
          , bio = profile.bio
          , email = pure profile.email
          }
    UpdateImage image -> self.setState _ { image = Just image }
    UpdateUsername username -> self.setState _ { username = V.Modified username }
    UpdateBio bio -> self.setState _ { bio = Just bio }
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
            res <- Request.makeSecureRequest (Apiary.Route :: UpdateUser) Apiary.none Apiary.none { user: validated }
            case res of
              Left _ -> self.setState _ { submitResponse = RemoteData.Failure (Object.singleton "unknown error:" [ "request failed" ]) }
              Right response ->
                response
                  # Variant.match
                      { ok:
                          \{ user } -> do
                            self.setState _ { submitResponse = RemoteData.Success unit }
                            updateProfile $ Record.delete (SProxy :: _ "token") user
                            navigate Home
                      , unprocessableEntity:
                          \{ errors } ->
                            self.setState _ { submitResponse = RemoteData.Failure errors }
                      }
    Logout -> do
      logout
      redirect Home

  render store props =
    let
      errors = validate store.state # unV identity (const mempty)
    in
      guard (isJust store.state.profile)
        $ container
            [ R.h1
                { className: "text-xs-center"
                , children:
                    [ R.text "Your Settings"
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
                                    { className: "form-control"
                                    , type: "text"
                                    , value: fromMaybe "" store.state.image
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
                                [ R.textarea
                                    { className: "form-control"
                                    , rows: 8
                                    , value: fromMaybe "" store.state.bio
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
  = { image :: Maybe String
    , username :: V.Validated String
    , bio :: Maybe String
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
  = { image :: Maybe String
    , username :: Username
    , bio :: Maybe String
    , email :: String
    , password :: String
    }

validate :: forall r. ValidationValues r -> V ValidationErrors ValidatedValues
validate values = ado
  username <-
    values.username
      # V.validate (V.toRecord (LR.prop (SProxy :: _ "username"))) \username -> do
          V.validateNonEmpty username
            `andThen`
              V.validateUsernameFormat
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
  in { image: values.image, username, bio: values.bio, email, password }
