module Conduit.Page.Editor (Props, mkEditorPage) where

import Prelude
import Apiary.Route (Route(..)) as Apiary
import Apiary.Types (none) as Apiary
import Conduit.Api.Endpoints (GetArticle, UpdateArticle, CreateArticle)
import Conduit.Api.Utils as Utils
import Conduit.Capability.Routing (navigate)
import Conduit.Component.App as App
import Conduit.Component.ResponseErrors (responseErrors)
import Conduit.Component.TagInput (tagInput)
import Conduit.Data.Route (Route(..))
import Conduit.Data.Slug (Slug)
import Conduit.Form.Validated as V
import Conduit.Form.Validator as F
import Control.Comonad (extract)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Lens.Record as LR
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Validation.Semigroup (andThen, toEither, unV)
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
  | UpdateTagList (Set String)
  | Submit

mkEditorPage :: App.Component Props
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
    , tagList: Set.empty
    , submitResponse: RemoteData.NotAsked
    }

  update self = case _ of
    Initialize ->
      for_ self.props.slug \slug -> do
        self.setState _ { article = RemoteData.Loading }
        res <- Utils.makeSecureRequest (Apiary.Route :: GetArticle) { slug } Apiary.none Apiary.none
        case res of
          Left error -> self.setState _ { article = RemoteData.Failure error }
          Right success ->
            success
              # Variant.match
                  { ok:
                      \{ article } ->
                        self.setState
                          _
                            { article = RemoteData.Success article
                            , title = pure article.title
                            , description = pure article.description
                            , body = pure article.body
                            , tagList = Set.fromFoldable article.tagList
                            }
                  , notFound: \_ -> navigate Home
                  }
    UpdateTitle title -> self.setState _ { title = V.Modified title }
    UpdateDescription description -> self.setState _ { description = V.Modified description }
    UpdateBody body -> self.setState _ { body = V.Modified body }
    UpdateTagList tagList -> self.setState _ { tagList = tagList }
    Submit -> do
      let
        state = V.setModified self.state
      case toEither (validate state) of
        Left _ -> self.setState (const state)
        Right validated -> do
          self.setState _ { submitResponse = RemoteData.Loading }
          res <- case self.props.slug of
            Nothing -> map Variant.expand <$> Utils.makeSecureRequest (Apiary.Route :: CreateArticle) Apiary.none Apiary.none { article: validated }
            Just slug -> map Variant.expand <$> Utils.makeSecureRequest (Apiary.Route :: UpdateArticle) { slug } Apiary.none { article: validated }
          case res of
            Left _ -> self.setState _ { submitResponse = RemoteData.Failure (Object.singleton "unknown error:" [ "request failed" ]) }
            Right success ->
              success
                # Variant.match
                    { ok:
                        \{ article } -> do
                          self.setState _ { submitResponse = RemoteData.Success unit }
                          navigate $ ViewArticle article.slug
                    , unprocessableEntity: \{ errors } -> self.setState _ { submitResponse = RemoteData.Failure errors }
                    }

  validate values = ado
    title <- values.title # V.validated (LR.prop (SProxy :: _ "title")) F.nonEmpty
    description <- values.description # V.validated (LR.prop (SProxy :: _ "description")) F.nonEmpty
    body <- values.body # V.validated (LR.prop (SProxy :: _ "body")) \body -> F.nonEmpty body `andThen` F.minimumLength 3
    in { title, description, body, tagList: Set.toUnfoldable values.tagList }

  render store props =
    let
      errors = validate store.state # unV identity (const mempty) :: { title :: _, description :: _, body :: _ }
    in
      guard (not $ RemoteData.isLoading store.state.article) container
        [ responseErrors store.state.submitResponse
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
                                , value: extract store.state.description
                                , placeholder: "What is this article about?"
                                , onChange: handler targetValue $ traverse_ $ store.dispatch <<< UpdateDescription
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
                                , value: extract store.state.body
                                , placeholder: "Write your article (in markdown)"
                                , onChange: handler targetValue $ traverse_ $ store.dispatch <<< UpdateBody
                                }
                            , guard (not $ Array.null errors.body) R.div
                                { className: "error-messages"
                                , children: errors.body <#> \error -> R.div_ [ R.text $ "Body " <> error ]
                                }
                            ]
                        }
                    , tagInput
                        { tags: store.state.tagList
                        , onChange: store.dispatch <<< UpdateTagList
                        }
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
                                { className: "col-md-6 offset-md-3 col-xs12"
                                , children
                                }
                            ]
                        }
                    ]
                }
            ]
        }
