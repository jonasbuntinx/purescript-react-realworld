module Conduit.Page.Article (Props, mkArticlePage) where

import Prelude
import Apiary.Route (Route(..)) as Apiary
import Apiary.Types (none) as Apiary
import Conduit.Api.Endpoints (CreateComment, DeleteArticle, DeleteComment, GetArticle, ListComments)
import Conduit.Api.Utils as Utils
import Conduit.Capability.Routing (navigate, toRouteURL)
import Conduit.Component.App as App
import Conduit.Component.Buttons (ButtonSize(..), favoriteButton, followButton)
import Conduit.Component.Link as Link
import Conduit.Data.Avatar as Avatar
import Conduit.Data.Comment (CommentId)
import Conduit.Data.Route (Route(..))
import Conduit.Data.Slug (Slug)
import Conduit.Data.Username as Username
import Conduit.Form.Validated as V
import Conduit.Form.Validator as F
import Conduit.Hook.Auth (useAuth)
import Conduit.Page.Utils (_article, _author, toggleFavorite, toggleFollow)
import Control.Comonad (extract)
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.Lens (preview, set)
import Data.Lens.Record as LR
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Data.Validation.Semigroup (toEither, unV)
import Data.Variant as Variant
import Foreign.Day (toDisplay)
import Foreign.NanoMarkdown (nmd)
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

mkArticlePage :: App.Component Props
mkArticlePage =
  App.component "ArticlePage" { init, update } \env store props -> React.do
    auth <- useAuth env
    React.useEffect props.slug do
      store.dispatch Initialize
      store.dispatch LoadComments
      mempty
    pure $ render env auth store props
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
                { ok: \{ article } -> self.setState _ { article = RemoteData.Success article }
                , notFound: \_ -> navigate Home
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
    ToggleFollow -> toggleFollow (preview _author self.state) (self.setState <<< set _author)
    ToggleFavorite -> toggleFavorite (preview _article self.state) (self.setState <<< set _article)
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

  loadComments self = do
    res <- Utils.makeRequest (Apiary.Route :: ListComments) { slug: self.props.slug } Apiary.none Apiary.none
    self.setState _ { comments = res # either RemoteData.Failure (Variant.match { ok: RemoteData.Success <<< _.comments }) }

  render env auth store props =
    let
      errors = validate store.state # unV identity (const mempty) :: { body :: _ }
    in
      store.state.article
        # RemoteData.maybe React.empty \article ->
            React.fragment
              [ container (banner env auth article store)
                  [ R.div
                      { className: "row article-content"
                      , children:
                          [ R.div
                              { className: "col-xs-12"
                              , children:
                                  [ R.div { dangerouslySetInnerHTML: { __html: nmd article.body } }
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
                      , children: [ articleMeta env auth article store ]
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
                                              , href: toRouteURL Login
                                              , onClick: env.routing.navigate Login
                                              , children: [ R.text "Sign in" ]
                                              }
                                          , R.text " or "
                                          , Link.link
                                              { className: ""
                                              , href: toRouteURL Register
                                              , onClick: env.routing.navigate Register
                                              , children: [ R.text "sign up" ]
                                              }
                                          , R.text " to add comments on this article."
                                          ]
                                  , (preview _Success store.state.comments)
                                      # maybe React.empty (React.fragment <<< commentList env auth store)
                                  ]
                              }
                          ]
                      }
                  ]
              ]

  articleMeta env auth article store =
    R.div
      { className: "article-meta"
      , children:
          [ Link.link
              { className: ""
              , href: toRouteURL $ Profile article.author.username
              , onClick: env.routing.navigate $ Profile article.author.username
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
                      , href: toRouteURL $ Profile article.author.username
                      , onClick: env.routing.navigate $ Profile article.author.username
                      , children: [ R.text $ Username.toString article.author.username ]
                      }
                  , R.span
                      { className: "date"
                      , children:
                          [ R.text $ toDisplay article.createdAt
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
                        , href: toRouteURL $ UpdateArticle article.slug
                        , onClick: env.routing.navigate $ UpdateArticle article.slug
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
                        , onClick: handler_ $ store.dispatch DeleteArticle
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

  commentList env auth store =
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
                        , href: toRouteURL $ Profile comment.author.username
                        , onClick: env.routing.navigate $ Profile comment.author.username
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
                        , href: toRouteURL $ Profile comment.author.username
                        , onClick: env.routing.navigate $ Profile comment.author.username
                        , children:
                            [ R.text $ Username.toString comment.author.username ]
                        }
                    , R.text " "
                    , R.span
                        { className: "date-posted"
                        , children:
                            [ R.text $ toDisplay comment.createdAt ]
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

  banner env auth article store =
    R.div
      { className: "banner"
      , children:
          [ R.div
              { className: "container"
              , children:
                  [ R.h1_ [ R.text article.title ]
                  , articleMeta env auth article store
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

  validate values = ado
    body <- values.body # V.validated (LR.prop (SProxy :: _ "body")) F.nonEmpty
    in { body }
