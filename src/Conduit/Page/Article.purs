module Conduit.Page.Article where

import Prelude
import Apiary.Route (Route(..)) as Apiary
import Apiary.Types (none) as Apiary
import Conduit.Api.Article (DeleteArticle, DeleteComment, FavoriteArticle, GetArticle, ListComments, UnfavoriteArticle, CreateComment)
import Conduit.Api.Profile (UnfollowProfile, FollowProfile)
import Conduit.Api.Utils as Utils
import Conduit.Component.App as App
import Conduit.Component.Buttons (ButtonSize(..), favoriteButton, followButton)
import Conduit.Component.Confirm as Confirm
import Conduit.Component.Link as Link
import Conduit.Data.Article (Article)
import Conduit.Data.Avatar as Avatar
import Conduit.Data.Comment (CommentId)
import Conduit.Data.Profile (Author)
import Conduit.Data.Route (Route(..))
import Conduit.Data.Slug (Slug)
import Conduit.Data.Username as Username
import Conduit.Effects.Routing (navigate)
import Conduit.Env (Env)
import Conduit.Form.Validated as V
import Conduit.Form.Validator as F
import Conduit.Hook.Auth (useAuth)
import Control.Comonad (extract)
import Data.Either (Either(..), either)
import Data.Foldable (for_, traverse_)
import Data.Lens (Traversal', preview, set)
import Data.Lens.Record as LR
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Data.Validation.Semigroup (V, toEither, unV)
import Data.Variant as Variant
import Foreign.Marked (marked)
import Foreign.Moment (Format(..), format)
import Foreign.Object as Object
import Network.RemoteData (_Success)
import Network.RemoteData as RemoteData
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks as React

type Props
  = { slug :: Slug
    }

data Action
  = Initialize
  | LoadComments
  | DeleteArticle
  | ToggleFollow
  | ToggleFavorite
  | UpdateBody String
  | DeleteComment CommentId
  | SubmitComment
  | ToggleModal Boolean

mkArticlePage :: App.Component Env Props
mkArticlePage =
  App.component "ArticlePage" { init, update } \env store props -> React.do
    auth <- useAuth env
    React.useEffect props.slug do
      store.dispatch Initialize
      store.dispatch LoadComments
      mempty
    pure $ render auth store props
  where
  init =
    { article: RemoteData.NotAsked
    , comments: RemoteData.NotAsked
    , body: pure ""
    , submitResponse: RemoteData.NotAsked
    , isModalOpen: false
    }

  update self = case _ of
    Initialize -> do
      self.setState _ { article = RemoteData.Loading }
      res <- Utils.makeRequest (Apiary.Route :: GetArticle) { slug: self.props.slug } Apiary.none Apiary.none
      case res of
        Left error -> self.setState _ { article = RemoteData.Failure error }
        Right response ->
          response
            # Variant.match
                { ok: \{ article } -> do self.setState _ { article = RemoteData.Success article }
                , notFound: \_ -> do navigate Home
                }
    LoadComments -> do
      self.setState _ { comments = RemoteData.Loading }
      loadComments self
    DeleteArticle -> do
      self.setState _ { submitResponse = RemoteData.Loading }
      res <- Utils.makeSecureRequest (Apiary.Route :: DeleteArticle) { slug: self.props.slug } Apiary.none Apiary.none
      case res of
        Left error -> self.setState _ { submitResponse = RemoteData.Failure (Object.singleton "unknown error:" [ "request failed" ]) }
        Right response ->
          response
            # Variant.match
                { ok:
                    \_ -> do
                      self.setState _ { submitResponse = RemoteData.Success unit, isModalOpen = false }
                      navigate Home
                }
    ToggleFollow -> do
      for_ (preview _author self.state) \{ username, following } -> do
        res <-
          if following then
            Utils.makeSecureRequest (Apiary.Route :: UnfollowProfile) { username } Apiary.none Apiary.none
          else
            Utils.makeSecureRequest (Apiary.Route :: FollowProfile) { username } Apiary.none Apiary.none
        for_ res $ Variant.match { ok: \{ profile } -> self.setState $ set _author profile }
    ToggleFavorite -> do
      for_ (preview _article self.state) \{ slug, favorited } -> do
        res <-
          if favorited then
            Utils.makeSecureRequest (Apiary.Route :: UnfavoriteArticle) { slug } Apiary.none Apiary.none
          else
            Utils.makeSecureRequest (Apiary.Route :: FavoriteArticle) { slug } Apiary.none Apiary.none
        for_ res $ Variant.match { ok: \{ article } -> self.setState $ set _article article }
    UpdateBody body -> self.setState _ { body = V.Modified body }
    DeleteComment id -> do
      self.setState _ { submitResponse = RemoteData.Loading }
      res <- Utils.makeSecureRequest (Apiary.Route :: DeleteComment) { slug: self.props.slug, id } Apiary.none Apiary.none
      case res of
        Left error -> self.setState _ { submitResponse = RemoteData.Failure (Object.singleton "unknown error:" [ "request failed" ]) }
        Right response ->
          response
            # Variant.match
                { ok:
                    \_ -> do
                      self.setState _ { submitResponse = RemoteData.Success unit }
                      loadComments self
                }
    SubmitComment ->
      let
        state = V.setModified self.state
      in
        case toEither (validate state) of
          Left _ -> self.setState (const state)
          Right validated -> do
            self.setState _ { submitResponse = RemoteData.Loading }
            res <- Utils.makeSecureRequest (Apiary.Route :: CreateComment) { slug: self.props.slug } Apiary.none { comment: validated }
            case res of
              Left _ -> self.setState _ { submitResponse = RemoteData.Failure (Object.singleton "unknown error:" [ "request failed" ]) }
              Right response ->
                response
                  # Variant.match
                      { ok:
                          \_ -> do
                            self.setState
                              _
                                { submitResponse = RemoteData.Success unit
                                , body = pure ""
                                }
                            loadComments self
                      }
    ToggleModal isOpen -> self.setState _ { isModalOpen = isOpen }

  loadComments self = do
    res <- Utils.makeRequest (Apiary.Route :: ListComments) { slug: self.props.slug } Apiary.none Apiary.none
    self.setState _ { comments = res # either RemoteData.Failure (Variant.match { ok: RemoteData.Success <<< _.comments }) }

  render auth store props =
    let
      errors = validate store.state # unV identity (const mempty)
    in
      store.state.article
        # RemoteData.maybe React.empty \article ->
            React.fragment
              [ container (banner auth article store)
                  [ R.div
                      { className: "row article-content"
                      , children:
                          [ R.div
                              { className: "col-xs-12"
                              , children:
                                  [ R.div
                                      { dangerouslySetInnerHTML:
                                          { __html: marked article.body
                                          }
                                      }
                                  , R.ul
                                      { className: "tag-list"
                                      , children:
                                          article.tagList
                                            <#> \tag ->
                                                R.li
                                                  { className: "tag-default tag-pill tag-outline"
                                                  , children: [ R.text tag ]
                                                  }
                                      }
                                  ]
                              }
                          ]
                      }
                  , R.hr {}
                  , R.div
                      { className: "article-actions"
                      , children: [ articleMeta auth article store ]
                      }
                  , R.div
                      { className: "row"
                      , children:
                          [ R.div
                              { className: "col-xs-12 col-md-8 offset-md-2"
                              , children:
                                  [ case auth of
                                      Just _ ->
                                        R.form
                                          { className: "card comment-form"
                                          , onSubmit: handler preventDefault $ const $ store.dispatch SubmitComment
                                          , children:
                                              [ R.div
                                                  { className: "card-block"
                                                  , children:
                                                      [ R.textarea
                                                          { className: "form-control"
                                                          , rows: 3
                                                          , value: extract store.state.body
                                                          , placeholder: "Write a comment..."
                                                          , onChange: handler targetValue $ traverse_ $ store.dispatch <<< UpdateBody
                                                          }
                                                      ]
                                                  }
                                              , R.div
                                                  { className: "card-footer"
                                                  , children:
                                                      [ R.img
                                                          { className: "comment-author-img"
                                                          , src: Avatar.toString $ maybe Avatar.blank (Avatar.withDefault <<< _.image) (_.profile =<< auth)
                                                          }
                                                      , R.button
                                                          { className: "btn btn-sm btn-primary"
                                                          , type: "submit"
                                                          , children: [ R.text "Post Comment" ]
                                                          }
                                                      ]
                                                  }
                                              ]
                                          }
                                      Nothing ->
                                        R.p_
                                          [ Link.link
                                              { className: ""
                                              , route: Login
                                              , children: [ R.text "Sign in" ]
                                              }
                                          , R.text " or "
                                          , Link.link
                                              { className: ""
                                              , route: Register
                                              , children: [ R.text "sign up" ]
                                              }
                                          , R.text " to add comments on this article."
                                          ]
                                  , (preview _Success store.state.comments)
                                      # maybe React.empty (React.fragment <<< commentList auth store)
                                  ]
                              }
                          ]
                      }
                  ]
              , Confirm.confirm
                  _
                    { isOpen = store.state.isModalOpen
                    , onCancel = store.dispatch $ ToggleModal false
                    , title = "Delete Article"
                    , children =
                      [ R.text $ "Are you sure you want to delete \"" <> article.title <> "\"?"
                      ]
                    , onConfirm = store.dispatch DeleteArticle
                    }
              ]

  articleMeta auth article store =
    R.div
      { className: "article-meta"
      , children:
          [ Link.link
              { className: ""
              , route: Profile article.author.username
              , children:
                  [ R.img
                      { src: Avatar.toString $ Avatar.withDefault article.author.image
                      , alt: Username.toString article.author.username
                      }
                  ]
              }
          , R.div
              { className: "info"
              , children:
                  [ Link.link
                      { className: "author"
                      , route: Profile article.author.username
                      , children: [ R.text $ Username.toString article.author.username ]
                      }
                  , R.span
                      { className: "date"
                      , children:
                          [ R.text $ format (Format "MMMM Do, YYYY") article.createdAt
                          ]
                      }
                  ]
              }
          , case _.username <$> auth of
              Just username
                | username == article.author.username ->
                  R.span_
                    [ Link.link
                        { className: "btn btn-outline-secondary btn-sm"
                        , route: UpdateArticle article.slug
                        , children:
                            [ R.i
                                { className: "ion-edit"
                                , children: []
                                }
                            , R.text " Edit Article"
                            ]
                        }
                    , R.text " "
                    , R.button
                        { className: "btn btn-outline-danger btn-sm"
                        , onClick: handler_ $ store.dispatch $ ToggleModal true
                        , children:
                            [ R.i
                                { className: "ion-trash-a"
                                , children: []
                                }
                            , R.text " Delete Article"
                            ]
                        }
                    ]
              _ ->
                R.span_
                  [ followButton
                      { following: article.author.following
                      , username: article.author.username
                      , onClick: handler_ $ store.dispatch ToggleFollow
                      }
                  , R.text " "
                  , favoriteButton
                      { size: Medium
                      , favorited: article.favorited
                      , count: article.favoritesCount
                      , onClick: handler_ $ store.dispatch ToggleFavorite
                      }
                  ]
          ]
      }

  commentList auth store =
    map \comment ->
      R.div
        { className: "card"
        , children:
            [ R.div
                { className: "card-block"
                , children:
                    [ R.p
                        { className: "card-text"
                        , children: [ R.text comment.body ]
                        }
                    ]
                }
            , R.div
                { className: "card-footer"
                , children:
                    [ Link.link
                        { className: "comment-author"
                        , route: Profile comment.author.username
                        , children:
                            [ R.img
                                { className: "comment-author-img"
                                , src: Avatar.toString $ Avatar.withDefault comment.author.image
                                }
                            ]
                        }
                    , R.text " "
                    , Link.link
                        { className: "comment-author"
                        , route: Profile comment.author.username
                        , children:
                            [ R.text $ Username.toString comment.author.username ]
                        }
                    , R.text " "
                    , R.span
                        { className: "date-posted"
                        , children:
                            [ R.text $ format (Format "MMMM Do, YYYY") comment.createdAt ]
                        }
                    , guard (Just comment.author.username == map _.username auth) R.span
                        { className: "mod-options"
                        , children:
                            [ R.i
                                { className: "ion-trash-a"
                                , onClick: handler_ $ store.dispatch $ DeleteComment comment.id
                                , children: []
                                }
                            ]
                        }
                    ]
                }
            ]
        }

  banner auth article store =
    R.div
      { className: "banner"
      , children:
          [ R.div
              { className: "container"
              , children:
                  [ R.h1_
                      [ R.text article.title ]
                  , articleMeta auth article store
                  ]
              }
          ]
      }

  container header children =
    R.div
      { className: "article-page"
      , children:
          [ header
          , R.div
              { className: "container page"
              , children
              }
          ]
      }

_author :: forall err r. Traversal' { article :: RemoteData.RemoteData err Article | r } Author
_author =
  LR.prop (SProxy :: _ "article")
    <<< RemoteData._Success
    <<< LR.prop (SProxy :: _ "author")

_article :: forall err r. Traversal' { article :: RemoteData.RemoteData err Article | r } Article
_article =
  LR.prop (SProxy :: _ "article")
    <<< RemoteData._Success

-- | Validation
type ValidationValues r
  = { body :: V.Validated String
    | r
    }

type ValidationErrors
  = { body :: Array String
    }

type ValidatedValues
  = { body :: String
    }

validate :: forall r. ValidationValues r -> V ValidationErrors ValidatedValues
validate values = ado
  body <-
    values.body
      # V.validated (LR.prop (SProxy :: _ "body")) \body -> do
          F.nonEmpty body
  in { body }
