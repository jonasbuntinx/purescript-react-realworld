module Conduit.Page.Article (Props, mkArticlePage) where

import Prelude
import Conduit.Capability.Api (createComment, deleteArticle, deleteComment, getArticle, listComments, toggleFavorite, toggleFollow)
import Conduit.Capability.Routing (navigate, redirect, toRouteURL)
import Conduit.Component.App as App
import Conduit.Component.Buttons (ButtonSize(..), favoriteButton, followButton)
import Conduit.Component.Link as Link
import Conduit.Data.Avatar as Avatar
import Conduit.Data.Comment (CommentId)
import Conduit.Data.Error (Error(..))
import Conduit.Data.Route (Route(..))
import Conduit.Data.Slug (Slug)
import Conduit.Data.Username as Username
import Conduit.Form.Validated as V
import Conduit.Form.Validator as F
import Conduit.Hook.Auth (useAuth)
import Conduit.Page.Utils (_article, _author)
import Control.Comonad (extract)
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Lens (preview, set)
import Data.Lens.Record as LR
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Data.Validation.Semigroup (toEither, unV)
import Foreign.Day (toDisplay)
import Foreign.NanoMarkdown (nmd)
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
    }

  update self = case _ of
    Initialize -> do
      self.setState _ { article = RemoteData.Loading }
      bind (getArticle self.props.slug) case _ of
        Left (NotFound _) -> redirect Home
        Left error -> self.setState _ { article = RemoteData.Failure error }
        Right article -> self.setState _ { article = RemoteData.Success article }
    LoadComments -> do
      self.setState _ { comments = RemoteData.Loading }
      listComments self.props.slug >>= \res -> self.setState _ { comments = RemoteData.fromEither res }
    DeleteArticle -> do
      self.setState _ { submitResponse = RemoteData.Loading }
      bind (deleteArticle self.props.slug) case _ of
        Right res -> do
          self.setState _ { submitResponse = RemoteData.Success res }
          navigate Home
        Left err -> self.setState _ { submitResponse = RemoteData.Failure err }
    ToggleFollow -> for_ (preview _author self.state) (toggleFollow >=> traverse_ (self.setState <<< set _author))
    ToggleFavorite -> for_ (preview _article self.state) (toggleFavorite >=> traverse_ (self.setState <<< set _article))
    UpdateBody body -> self.setState _ { body = V.Modified body }
    DeleteComment id -> do
      self.setState _ { submitResponse = RemoteData.Loading }
      bind (deleteComment self.props.slug id) case _ of
        Right _ -> do
          self.setState _ { submitResponse = RemoteData.Success unit }
          listComments self.props.slug >>= \res -> self.setState _ { comments = RemoteData.fromEither res }
        Left err -> self.setState _ { submitResponse = RemoteData.Failure err }
    SubmitComment ->
      let
        state = V.setModified self.state
      in
        case toEither (validate state) of
          Left _ -> self.setState (const state)
          Right validated -> do
            self.setState _ { submitResponse = RemoteData.Loading }
            bind (createComment self.props.slug validated) case _ of
              Right _ -> do
                self.setState _ { submitResponse = RemoteData.Success unit, body = pure "" }
                listComments self.props.slug >>= \res -> self.setState _ { comments = RemoteData.fromEither res }
              Left err -> self.setState _ { submitResponse = RemoteData.Failure err }

  validate values = ado
    body <- values.body # V.validated (LR.prop (SProxy :: _ "body")) F.nonEmpty
    in { body }

  render env auth store props =
    let
      errors = validate store.state # unV identity (const mempty) :: { body :: _ }
    in
      store.state.article
        # RemoteData.maybe React.empty \article ->
            React.fragment
              [ container (banner article)
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
                      , children: [ articleMeta article ]
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
                                              , onClick: env.router.navigate Login
                                              , children: [ R.text "Sign in" ]
                                              }
                                          , R.text " or "
                                          , Link.link
                                              { className: ""
                                              , href: toRouteURL Register
                                              , onClick: env.router.navigate Register
                                              , children: [ R.text "sign up" ]
                                              }
                                          , R.text " to add comments on this article."
                                          ]
                                  , (preview _Success store.state.comments)
                                      # maybe React.empty (React.fragment <<< commentList)
                                  ]
                              }
                          ]
                      }
                  ]
              ]
    where
    articleMeta article =
      R.div
        { className: "article-meta"
        , children:
            [ Link.link
                { className: ""
                , href: toRouteURL $ Profile article.author.username
                , onClick: env.router.navigate $ Profile article.author.username
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
                        , onClick: env.router.navigate $ Profile article.author.username
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
                          , onClick: env.router.navigate $ UpdateArticle article.slug
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

    commentList =
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
                          , onClick: env.router.navigate $ Profile comment.author.username
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
                          , onClick: env.router.navigate $ Profile comment.author.username
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

    banner article =
      R.div
        { className: "banner"
        , children:
            [ R.div
                { className: "container"
                , children:
                    [ R.h1_ [ R.text article.title ]
                    , articleMeta article
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
