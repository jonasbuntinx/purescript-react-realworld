module Conduit.Page.Editor (Props, mkEditorPage) where

import Prelude
import Conduit.Api.Client (isNotFound)
import Conduit.Capability.Halo (class MonadHalo, component)
import Conduit.Capability.Resource.Article (class ArticleRepository, getArticle, submitArticle)
import Conduit.Capability.Routing (class MonadRouting, navigate, redirect)
import Conduit.Component.ResponseErrors (responseErrors)
import Conduit.Component.TagInput (tagInput)
import Conduit.Data.Route (Route(..))
import Conduit.Data.Slug (Slug)
import Conduit.Form.Validated as V
import Conduit.Form.Validator as F
import Control.Comonad (extract)
import Control.Monad.State (modify_, get, put)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Lens.Record as LR
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Validation.Semigroup (andThen, toEither, validation)
import Network.RemoteData as RemoteData
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks as React
import React.Halo as Halo

type Props =
  { slug :: Maybe Slug
  }

data Action
  = Initialize
  | UpdateTitle String
  | UpdateDescription String
  | UpdateBody String
  | UpdateTagList (Set String)
  | Submit

mkEditorPage ::
  forall m.
  ArticleRepository m =>
  MonadRouting m =>
  MonadHalo m =>
  m (Props -> React.JSX)
mkEditorPage = component "SettingsPage" { context, initialState, eval, render }
  where
  context _ = pure unit

  initialState _ _ =
    { article: RemoteData.NotAsked
    , title: pure ""
    , description: pure ""
    , body: pure ""
    , tagList: Set.empty
    , submitResponse: RemoteData.NotAsked
    }

  eval =
    Halo.mkEval
      _
        { onInitialize = \_ -> Just Initialize
        , onUpdate = \prev next -> if (prev.props.slug /= next.props.slug) then Just Initialize else Nothing
        , onAction = handleAction
        }

  handleAction = case _ of
    Initialize -> do
      props <- Halo.props
      case props.slug of
        Nothing -> put $ initialState props unit
        Just slug -> do
          modify_ _ { article = RemoteData.Loading }
          response <- getArticle slug
          modify_ _ { article = RemoteData.fromEither response }
          case response of
            Left _ -> when (isNotFound response) do redirect Home
            Right article -> do
              modify_
                _
                  { title = pure article.title
                  , description = pure article.description
                  , body = pure article.body
                  , tagList = Set.fromFoldable article.tagList
                  }
    UpdateTitle title -> modify_ _ { title = V.Modified title }
    UpdateDescription description -> modify_ _ { description = V.Modified description }
    UpdateBody body -> modify_ _ { body = V.Modified body }
    UpdateTagList tagList -> modify_ _ { tagList = tagList }
    Submit -> do
      props <- Halo.props
      state <- V.setModified <$> get
      case toEither (validate state) of
        Left _ -> modify_ (const state)
        Right validated -> do
          modify_ _ { submitResponse = RemoteData.Loading }
          response <- submitArticle props.slug validated
          case response of
            Right article -> do
              modify_ _ { submitResponse = RemoteData.Success unit }
              navigate $ ViewArticle article.slug
            Left err -> modify_ _ { submitResponse = RemoteData.Failure err }

  validate values = ado
    title <- values.title # V.validated (LR.prop (SProxy :: _ "title")) F.nonEmpty
    description <- values.description # V.validated (LR.prop (SProxy :: _ "description")) F.nonEmpty
    body <- values.body # V.validated (LR.prop (SProxy :: _ "body")) \body -> F.nonEmpty body `andThen` F.minimumLength 3
    in { title, description, body, tagList: Set.toUnfoldable values.tagList }

  render { state, send } =
    let
      errors = validate state # validation identity (const mempty) :: { title :: _, description :: _, body :: _ }
    in
      guard (not $ RemoteData.isLoading state.article) container
        [ responseErrors state.submitResponse
        , R.form
            { children:
                [ R.fieldset_
                    [ R.fieldset
                        { className: "form-group"
                        , children:
                            [ R.input
                                { className: "form-control"
                                , type: "text"
                                , value: extract state.title
                                , placeholder: "Article title"
                                , onChange: handler targetValue $ traverse_ $ send <<< UpdateTitle
                                }
                            , guard (not $ Array.null errors.title) R.div
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
                                , value: extract state.description
                                , placeholder: "What is this article about?"
                                , onChange: handler targetValue $ traverse_ $ send <<< UpdateDescription
                                }
                            , guard (not $ Array.null errors.description) R.div
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
                                , value: extract state.body
                                , placeholder: "Write your article (in markdown)"
                                , onChange: handler targetValue $ traverse_ $ send <<< UpdateBody
                                }
                            , guard (not $ Array.null errors.body) R.div
                                { className: "error-messages"
                                , children: errors.body <#> \error -> R.div_ [ R.text $ "Body " <> error ]
                                }
                            ]
                        }
                    , tagInput
                        { tags: state.tagList
                        , onChange: send <<< UpdateTagList
                        }
                    , R.button
                        { className: "btn btn-primary pull-xs-right"
                        , type: "button"
                        , onClick: handler_ $ send Submit
                        , children: [ R.text "Publish article" ]
                        }
                    ]
                ]
            }
        ]
    where
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
                                { className: "col-md-6 offset-md-3 col-xs-12"
                                , children
                                }
                            ]
                        }
                    ]
                }
            ]
        }
