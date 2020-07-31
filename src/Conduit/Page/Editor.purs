module Conduit.Page.Editor where

import Prelude
import Apiary.Route (Route(..)) as Apiary
import Apiary.Types (none) as Apiary
import Conduit.Api.Article (GetArticle, UpdateArticle, CreateArticle)
import Conduit.Api.Request as Request
import Conduit.Component.App as App
import Conduit.Data.Route (Route(..))
import Conduit.Data.Slug (Slug)
import Conduit.Data.Validation as V
import Conduit.Effects.Routing (navigate)
import Conduit.Env (Env)
import Control.Comonad (extract)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Lens.Record as LR
import Data.Maybe (Maybe(..))
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

type Props
  = { slug :: Maybe Slug
    }

data Action
  = Initialize
  | UpdateTitle String
  | UpdateDescription String
  | UpdateBody String
  | UpdateTagList (Array String)
  | Submit

mkEditorPage :: App.Component Env Props
mkEditorPage =
  App.component "SettingsPage" { init, update } \env store props -> React.do
    React.useEffect props.slug do
      store.dispatch Initialize
      mempty
    pure $ render store props
  where
  init =
    { article: RemoteData.NotAsked
    , title: pure ""
    , description: pure ""
    , body: pure ""
    , tagList: []
    , submitResponse: RemoteData.NotAsked
    }

  update self = case _ of
    Initialize ->
      for_ self.props.slug \slug -> do
        self.setState _ { article = RemoteData.Loading }
        res <- Request.makeSecureRequest (Apiary.Route :: GetArticle) { slug } Apiary.none Apiary.none
        case res of
          Left error -> self.setState _ { article = RemoteData.Failure error }
          Right response ->
            response
              # Variant.match
                  { ok:
                      \{ article } -> do
                        self.setState
                          _
                            { article = RemoteData.Success article
                            , title = pure article.title
                            , description = pure article.description
                            , body = pure article.body
                            , tagList = article.tagList
                            }
                  }
    UpdateTitle title -> self.setState _ { title = V.Modified title }
    UpdateDescription description -> self.setState _ { description = V.Modified description }
    UpdateBody body -> self.setState _ { body = V.Modified body }
    UpdateTagList tagList -> self.setState _ { tagList = tagList }
    Submit ->
      let
        state = V.setModified self.state
      in
        case toEither (validate state) of
          Left _ -> self.setState (const state)
          Right validated -> do
            self.setState _ { submitResponse = RemoteData.Loading }
            res <- case self.props.slug of
              Nothing -> map Variant.expand <$> Request.makeSecureRequest (Apiary.Route :: CreateArticle) Apiary.none Apiary.none { article: validated }
              Just slug -> map Variant.expand <$> Request.makeSecureRequest (Apiary.Route :: UpdateArticle) { slug } Apiary.none { article: validated }
            case res of
              Left _ -> self.setState _ { submitResponse = RemoteData.Failure (Object.singleton "unknown error:" [ "request failed" ]) }
              Right response ->
                response
                  # Variant.match
                      { ok:
                          \{ article } -> do
                            self.setState _ { submitResponse = RemoteData.Success unit }
                            navigate $ ViewArticle article.slug
                      , unprocessableEntity:
                          \{ errors } ->
                            self.setState _ { submitResponse = RemoteData.Failure errors }
                      }

  render store props =
    let
      errors = validate store.state # unV identity (const mempty)
    in
      guard (not $ RemoteData.isLoading store.state.article)
        $ container
            [ case store.state.submitResponse of
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
                                    , value: extract store.state.title
                                    , placeholder: "Article title"
                                    , onChange: handler targetValue $ traverse_ $ store.dispatch <<< UpdateTitle
                                    }
                                , guard (not $ Array.null errors.title)
                                    $ R.div
                                        { className: "error-messages"
                                        , children: errors.title <#> \error -> R.div_ [ R.text $ "Name " <> error ]
                                        }
                                ]
                            }
                        , R.fieldset
                            { className: "form-group"
                            , children:
                                [ R.input
                                    { className: "form-control"
                                    , type: "text"
                                    , value: extract store.state.description
                                    , placeholder: "What is this article about?"
                                    , onChange: handler targetValue $ traverse_ $ store.dispatch <<< UpdateDescription
                                    }
                                , guard (not $ Array.null errors.description)
                                    $ R.div
                                        { className: "error-messages"
                                        , children: errors.description <#> \error -> R.div_ [ R.text $ "Description " <> error ]
                                        }
                                ]
                            }
                        , R.fieldset
                            { className: "form-group"
                            , children:
                                [ R.textarea
                                    { className: "form-control"
                                    , rows: 8
                                    , value: extract store.state.body
                                    , placeholder: "Write your article (in markdown)"
                                    , onChange: handler targetValue $ traverse_ $ store.dispatch <<< UpdateBody
                                    }
                                , guard (not $ Array.null errors.body)
                                    $ R.div
                                        { className: "error-messages"
                                        , children: errors.body <#> \error -> R.div_ [ R.text $ "Body " <> error ]
                                        }
                                ]
                            }
                        -- , R.fieldset
                        --     { className: "form-group"
                        --     , children:
                        --         [ R.input
                        --             { className: "form-control"
                        --             , type: "text"
                        --             , value: fromMaybe "" store.state.tagList
                        --             , placeholder: "Enter tags"
                        --             , onChange: handler targetValue $ traverse_ $ store.dispatch <<< UpdateTagList
                        --             }
                        --         ]
                        --     }
                        , R.button
                            { className: "btn btn-primary pull-xs-right"
                            , type: "button"
                            , onClick: handler_ $ store.dispatch Submit
                            , children: [ R.text "Publish article" ]
                            }
                        ]
                    ]
                }
            ]

  container children =
    R.div
      { className: "editor-page"
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
  = { title :: V.Validated String
    , description :: V.Validated String
    , body :: V.Validated String
    , tagList :: Array String
    | r
    }

type ValidationErrors
  = { title :: Array String
    , description :: Array String
    , body :: Array String
    }

type ValidatedValues
  = { title :: String
    , description :: String
    , body :: String
    , tagList :: Array String
    }

validate :: forall r. ValidationValues r -> V ValidationErrors ValidatedValues
validate values = ado
  title <-
    values.title
      # V.validate (V.toRecord (LR.prop (SProxy :: _ "title"))) \title -> do
          V.validateNonEmpty title
  description <-
    values.description
      # V.validate (V.toRecord (LR.prop (SProxy :: _ "description"))) \description -> do
          V.validateNonEmpty description
  body <-
    values.body
      # V.validate (V.toRecord (LR.prop (SProxy :: _ "body"))) \body -> do
          V.validateNonEmpty body
            `andThen`
              V.validateMinimumLength 3
  in { title, description, body, tagList: values.tagList }
