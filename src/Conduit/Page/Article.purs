module Conduit.Page.Article (Props, makeArticlePage) where

import Prelude
import Conduit.Capability.Api (createComment, deleteArticle, deleteComment, getArticle, listComments, toggleFavorite, toggleFollow)
import Conduit.Capability.Routing (navigate, redirect)
import Conduit.Component.Buttons (ButtonSize(..), favoriteButton, followButton)
import Conduit.Component.Link as Link
import Conduit.Component.Store as Store
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
import Control.Monad.State (modify_)
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

makeArticlePage :: Store.Component Props
makeArticlePage =
  Store.component "ArticlePage" { initialState, update } \store -> React.do
    auth <- useAuth store.env
    React.useEffect store.props.slug do
      store.send Initialize
      store.send LoadComments
      mempty
    pure $ render auth store
  where
  initialState =
    { article: RemoteData.NotAsked
    , comments: RemoteData.NotAsked
    , body: pure ""
    , submitResponse: RemoteData.NotAsked
    }

  update self = case _ of
    Initialize -> do
      modify_ _ { article = RemoteData.Loading }
      bind (getArticle self.props.slug) case _ of
        Left (NotFound _) -> redirect Home
        Left error -> modify_ _ { article = RemoteData.Failure error }
        Right article -> modify_ _ { article = RemoteData.Success article }
    LoadComments -> do
      modify_ _ { comments = RemoteData.Loading }
      listComments self.props.slug >>= \res -> modify_ _ { comments = RemoteData.fromEither res }
    DeleteArticle -> do
      modify_ _ { submitResponse = RemoteData.Loading }
      bind (deleteArticle self.props.slug) case _ of
        Right res -> do
          modify_ _ { submitResponse = RemoteData.Success res }
          navigate Home
        Left err -> modify_ _ { submitResponse = RemoteData.Failure err }
    ToggleFollow -> for_ (preview _author self.state) (toggleFollow >=> traverse_ (modify_ <<< set _author))
    ToggleFavorite -> for_ (preview _article self.state) (toggleFavorite >=> traverse_ (modify_ <<< set _article))
    UpdateBody body -> modify_ _ { body = V.Modified body }
    DeleteComment id -> do
      modify_ _ { submitResponse = RemoteData.Loading }
      bind (deleteComment self.props.slug id) case _ of
        Right _ -> do
          modify_ _ { submitResponse = RemoteData.Success unit }
          listComments self.props.slug >>= \res -> modify_ _ { comments = RemoteData.fromEither res }
        Left err -> modify_ _ { submitResponse = RemoteData.Failure err }
    SubmitComment ->
      let
        state = V.setModified self.state
      in
        case toEither (validate state) of
          Left _ -> modify_ (const state)
          Right validated -> do
            modify_ _ { submitResponse = RemoteData.Loading }
            bind (createComment self.props.slug validated) case _ of
              Right _ -> do
                modify_ _ { submitResponse = RemoteData.Success unit, body = pure "" }
                listComments self.props.slug >>= \res -> modify_ _ { comments = RemoteData.fromEither res }
              Left err -> modify_ _ { submitResponse = RemoteData.Failure err }

  validate values = ado
    body <- values.body # V.validated (LR.prop (SProxy :: _ "body")) F.nonEmpty
    in { body }

  render auth { env, props, state, send } =
    let
      errors = validate state # unV identity (const mempty) :: { body :: _ }
    in
      state.article
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
                                          , onSubmit: handler preventDefault $ const $ send SubmitComment
                                          , children:
                                              [ R.div
                                                  { className: "card-block"
                                                  , children:
                                                      [ R.textarea
                                                          { className: "form-control"
                                                          , rows: 3
                                                          , value: extract state.body
                                                          , placeholder: "Write a comment..."
                                                          , onChange: handler targetValue $ traverse_ $ send <<< UpdateBody
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
                                              , onClick: env.router.navigate
                                              , children: [ R.text "Sign in" ]
                                              }
                                          , R.text " or "
                                          , Link.link
                                              { className: ""
                                              , route: Register
                                              , onClick: env.router.navigate
                                              , children: [ R.text "sign up" ]
                                              }
                                          , R.text " to add comments on this article."
                                          ]
                                  , (preview _Success state.comments)
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
                , route: Profile article.author.username
                , onClick: env.router.navigate
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
                        , onClick: env.router.navigate
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
                          , route: UpdateArticle article.slug
                          , onClick: env.router.navigate
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
                          , onClick: handler_ $ send DeleteArticle
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
                        , onClick: handler_ $ send ToggleFollow
                        }
                    , R.text " "
                    , favoriteButton
                        { size: Medium
                        , favorited: article.favorited
                        , count: article.favoritesCount
                        , onClick: handler_ $ send ToggleFavorite
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
                          , route: Profile comment.author.username
                          , onClick: env.router.navigate
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
                          , onClick: env.router.navigate
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
                                  , onClick: handler_ $ send $ DeleteComment comment.id
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
