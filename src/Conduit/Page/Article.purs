module Conduit.Page.Article (Props, mkArticlePage) where

import Prelude
import Conduit.Capability.Auth (readAuth, readAuthEvent)
import Conduit.Capability.Resource.Article (deleteArticle, getArticle, toggleFavorite)
import Conduit.Capability.Resource.Comment (createComment, deleteComment, listComments)
import Conduit.Capability.Resource.Profile (toggleFollow)
import Conduit.Capability.Routing (navigate, redirect)
import Conduit.Component.App as App
import Conduit.Component.Buttons (ButtonSize(..), favoriteButton, followButton)
import Conduit.Component.Link as Link
import Conduit.Data.Auth (Auth)
import Conduit.Data.Avatar as Avatar
import Conduit.Data.Comment (CommentId)
import Conduit.Data.Error (Error(..))
import Conduit.Data.Route (Route(..))
import Conduit.Data.Slug (Slug)
import Conduit.Data.Username as Username
import Conduit.Form.Validated (Validated)
import Conduit.Form.Validated as V
import Conduit.Form.Validator as F
import Conduit.Page.Utils (_article, _author)
import Control.Comonad (extract)
import Control.Parallel (parTraverse_)
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Lens (preview, set)
import Data.Lens.Record as LR
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Data.Validation.Semigroup (V, toEither)
import Foreign.Day (toDisplay)
import Foreign.NanoMarkdown (nmd)
import Network.RemoteData (_Success)
import Network.RemoteData as RemoteData
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault, targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks as React
import React.Halo as Halo

type Props
  = { slug :: Slug
    }

data Action
  = Initialize (Array Action)
  | SubscribeToAuth
  | UpdateAuth (Maybe Auth)
  | Navigate Route
  | LoadArticle
  | LoadComments
  | DeleteArticle
  | ToggleFollow
  | ToggleFavorite
  | UpdateBody String
  | DeleteComment CommentId
  | SubmitComment

mkArticlePage :: App.Component Props
mkArticlePage = App.component "ArticlePage" { initialState, eval, render }
  where
  initialState =
    { auth: Nothing
    , article: RemoteData.NotAsked
    , comments: RemoteData.NotAsked
    , body: pure ""
    , submitResponse: RemoteData.NotAsked
    }

  eval =
    Halo.mkEval
      _
        { onInitialize = \_ -> Just $ Initialize [ SubscribeToAuth, LoadArticle, LoadComments ]
        , onUpdate = \prev next -> Just $ Initialize $ guard (prev.slug /= next.slug) [ LoadArticle, LoadComments ]
        , onAction = handleAction
        }

  handleAction = case _ of
    Initialize actions -> parTraverse_ handleAction actions
    SubscribeToAuth -> do
      auth <- readAuth
      handleAction $ UpdateAuth auth
      authEvent <- readAuthEvent
      void $ Halo.subscribe $ map UpdateAuth authEvent
    UpdateAuth auth -> Halo.modify_ _ { auth = auth }
    Navigate route -> navigate route
    LoadArticle -> do
      props <- Halo.props
      Halo.modify_ _ { article = RemoteData.Loading }
      response <- getArticle props.slug
      case response of
        Left (NotFound _) -> redirect Home
        Left error -> Halo.modify_ _ { article = RemoteData.Failure error }
        Right article -> Halo.modify_ _ { article = RemoteData.Success article }
    LoadComments -> do
      props <- Halo.props
      Halo.modify_ _ { comments = RemoteData.Loading }
      response <- listComments props.slug
      Halo.modify_ _ { comments = RemoteData.fromEither response }
    DeleteArticle -> do
      props <- Halo.props
      Halo.modify_ _ { submitResponse = RemoteData.Loading }
      response <- deleteArticle props.slug
      case response of
        Right res -> do
          Halo.modify_ _ { submitResponse = RemoteData.Success res }
          navigate Home
        Left err -> Halo.modify_ _ { submitResponse = RemoteData.Failure err }
    ToggleFollow -> do
      state <- Halo.get
      for_ (preview _author state) (toggleFollow >=> traverse_ (Halo.modify_ <<< set _author))
    ToggleFavorite -> do
      state <- Halo.get
      for_ (preview _article state) (toggleFavorite >=> traverse_ (Halo.modify_ <<< set _article))
    UpdateBody body -> Halo.modify_ _ { body = V.Modified body }
    DeleteComment id -> do
      props <- Halo.props
      Halo.modify_ _ { submitResponse = RemoteData.Loading }
      response <- deleteComment props.slug id
      case response of
        Right _ -> do
          Halo.modify_ _ { submitResponse = RemoteData.Success unit }
          response' <- listComments props.slug
          Halo.modify_ _ { comments = RemoteData.fromEither response' }
        Left err -> Halo.modify_ _ { submitResponse = RemoteData.Failure err }
    SubmitComment -> do
      props <- Halo.props
      state <- V.setModified <$> Halo.get
      case toEither (validate state) of
        Left _ -> Halo.modify_ (const state)
        Right validated -> do
          Halo.modify_ _ { submitResponse = RemoteData.Loading }
          response <- createComment props.slug validated
          case response of
            Right _ -> do
              Halo.modify_ _ { submitResponse = RemoteData.Success unit, body = pure "" }
              response' <- listComments props.slug
              Halo.modify_ _ { comments = RemoteData.fromEither response' }
            Left err -> Halo.modify_ _ { submitResponse = RemoteData.Failure err }

  validate :: forall r. { body :: Validated String | r } -> V { body :: Array String } { body :: String }
  validate values = ado
    body <- values.body # V.validated (LR.prop (SProxy :: _ "body")) F.nonEmpty
    in { body }

  render { state, send } =
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
                                [ case state.auth of
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
                                                        , src: Avatar.toString $ maybe Avatar.blank (Avatar.withDefault <<< _.image) (_.user =<< state.auth)
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
                                            , onClick: send <<< Navigate
                                            , children: [ R.text "Sign in" ]
                                            }
                                        , R.text " or "
                                        , Link.link
                                            { className: ""
                                            , route: Register
                                            , onClick: send <<< Navigate
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
                , onClick: send <<< Navigate
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
                        , onClick: send <<< Navigate
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
            , case _.username <$> state.auth of
                Just username
                  | username == article.author.username ->
                    R.span_
                      [ Link.link
                          { className: "btn btn-outline-secondary btn-sm"
                          , route: UpdateArticle article.slug
                          , onClick: send <<< Navigate
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
                          , onClick: send <<< Navigate
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
                          , onClick: send <<< Navigate
                          , children:
                              [ R.text $ Username.toString comment.author.username ]
                          }
                      , R.text " "
                      , R.span
                          { className: "date-posted"
                          , children:
                              [ R.text $ toDisplay comment.createdAt ]
                          }
                      , guard (Just comment.author.username == map _.username state.auth) R.span
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
