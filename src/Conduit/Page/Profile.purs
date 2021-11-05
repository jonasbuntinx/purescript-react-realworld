module Conduit.Page.Profile (Props, Tab(..), mkProfilePage) where

import Prelude

import Conduit.Api.Client (isNotFound)
import Conduit.Capability.Auth (class MonadAuth)
import Conduit.Capability.Auth as Auth
import Conduit.Capability.Halo (class MonadHalo, component)
import Conduit.Capability.Resource.Article (class ArticleRepository, listArticles, toggleFavorite)
import Conduit.Capability.Resource.Profile (class ProfileRepository, getProfile, toggleFollow)
import Conduit.Capability.Routing (class MonadRouting)
import Conduit.Capability.Routing as Routing
import Conduit.Component.ArticleList (articleList)
import Conduit.Component.Buttons (followButton)
import Conduit.Component.Pagination (pagination)
import Conduit.Component.Tabs as Tabs
import Conduit.Data.Article (defaultArticlesQuery)
import Conduit.Data.Auth (Auth)
import Conduit.Data.Avatar as Avatar
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username (Username)
import Conduit.Data.Username as Username
import Conduit.Page.Utils (_articles, _profile)
import Control.Monad.State (modify_, get)
import Control.Parallel (parTraverse_)
import Data.Foldable (for_, traverse_)
import Data.Lens (preview, set)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (guard)
import Network.RemoteData as RemoteData
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks as React
import React.Halo as Halo

type Props
  = { username :: Username
    , tab :: Tab
    }

data Tab
  = Published
  | Favorited

derive instance Eq Tab

data Action
  = Initialize
  | OnPropsUpdate Props Props
  | UpdateAuth (Maybe Auth)
  | Navigate Route
  | LoadProfile
  | LoadArticles { offset :: Int, limit :: Int }
  | ToggleFavorite Int
  | ToggleFollow

mkProfilePage ::
  forall m.
  MonadAuth m =>
  MonadRouting m =>
  ProfileRepository m =>
  ArticleRepository m =>
  MonadHalo m =>
  m (Props -> React.JSX)
mkProfilePage = component "ProfilePage" { context, initialState, eval, render }
  where
  context _ = pure unit

  initialState _ _ =
    { auth: Nothing
    , profile: RemoteData.NotAsked
    , articles: RemoteData.NotAsked
    , pagination: initialPagination
    }

  initialPagination =
    { offset: 0
    , limit: 5
    }

  eval =
    Halo.mkEval
      _
        { onInitialize = \_ -> Just Initialize
        , onUpdate = \prev next -> Just $ OnPropsUpdate prev.props next.props
        , onAction = handleAction
        }

  handleAction = case _ of
    Initialize -> do
      handleAction <<< UpdateAuth =<< Auth.read
      Auth.subscribe UpdateAuth
      parTraverse_ handleAction
        [ LoadProfile
        , LoadArticles initialPagination
        ]
    OnPropsUpdate prev next -> do
      let
        reloadProfile = prev.username /= next.username

        reloadArticles = reloadProfile || prev.tab /= next.tab

        actions =
          join
            [ guard reloadProfile [ LoadProfile ]
            , guard reloadArticles [ LoadArticles initialPagination ]
            ]
      parTraverse_ handleAction actions
    UpdateAuth auth -> modify_ _ { auth = auth }
    Navigate route -> Routing.navigate route
    LoadProfile -> do
      props <- Halo.props
      modify_ _ { profile = RemoteData.Loading }
      response <- getProfile props.username
      modify_ _ { profile = RemoteData.fromEither response }
      when (isNotFound response) do
        Routing.redirect Home
    LoadArticles pagination -> do
      props <- Halo.props
      let
        query = defaultArticlesQuery { offset = Just pagination.offset, limit = Just pagination.limit }
      modify_ _ { articles = RemoteData.Loading, pagination = pagination }
      response <-
        listArticles case props.tab of
          Published -> query { author = Just props.username }
          Favorited -> query { favorited = Just props.username }
      modify_ _ { articles = RemoteData.fromEither response }
    ToggleFavorite ix -> do
      state <- get
      for_ (preview (_articles ix) state) (toggleFavorite >=> traverse_ (modify_ <<< set (_articles ix)))
    ToggleFollow -> do
      state <- get
      for_ (preview _profile state) (toggleFollow >=> traverse_ (modify_ <<< set _profile))

  render { props, state, send } =
    guard (RemoteData.isSuccess state.profile) container userInfo
      [ Tabs.tabs
          { className: "articles-toggle"
          , selectedTab: Just props.tab
          , tabs:
              [ { id: Published
                , label: R.text "Published Articles"
                , disabled: false
                , content: tabContent
                }
              , { id: Favorited
                , label: R.text "Favorited Articles"
                , disabled: false
                , content: tabContent
                }
              ]
          , onChange:
              case _ of
                Published -> send $ Navigate $ Profile props.username
                Favorited -> send $ Navigate $ Favorites props.username
          }
      ]
    where
    tabContent =
      R.div_
        [ articleList
            { articles: state.articles <#> _.articles
            , onNavigate: send <<< Navigate
            , onFavoriteToggle: send <<< ToggleFavorite
            }
        , state.articles
            # RemoteData.maybe React.empty \{ articlesCount } ->
                pagination
                  { offset: state.pagination.offset
                  , limit: state.pagination.limit
                  , totalCount: articlesCount
                  , onChange: send <<< LoadArticles
                  , focusWindow: 3
                  , marginPages: 1
                  }
        ]

    userInfo =
      R.div
        { className: "user-info"
        , children:
            [ R.div
                { className: "container"
                , children:
                    [ R.div
                        { className: "row"
                        , children:
                            [ R.div
                                { className: "col-xs-12 col-md-10 offset-md-1"
                                , children:
                                    [ R.img
                                        { className: "user-img"
                                        , src: Avatar.toString $ RemoteData.maybe Avatar.blank (Avatar.withDefault <<< _.image) state.profile
                                        }
                                    , R.h4_ [ R.text $ Username.toString props.username ]
                                    , maybe React.empty (\bio -> R.p_ [ R.text bio ]) (RemoteData.toMaybe state.profile >>= _.bio)
                                    , if (Just props.username == map _.username state.auth) then
                                        R.button
                                          { className: "btn btn-sm action-btn btn-outline-secondary"
                                          , onClick: handler_ $ send $ Navigate Settings
                                          , children:
                                              [ R.i
                                                  { className: "ion-gear-a"
                                                  , children: []
                                                  }
                                              , R.text $ " Edit Profile Settings"
                                              ]
                                          }
                                      else
                                        followButton
                                          { following: fromMaybe false (RemoteData.toMaybe state.profile >>= _.following)
                                          , username: props.username
                                          , onClick: handler_ $ send ToggleFollow
                                          }
                                    ]
                                }
                            ]
                        }
                    ]
                }
            ]
        }

    container header children =
      R.div
        { className: "profile-page"
        , children:
            [ header
            , R.div
                { className: "container"
                , children:
                    [ R.div
                        { className: "row"
                        , children:
                            [ R.div
                                { className: "col-xs-12 col-md-10 offset-md-1"
                                , children
                                }
                            ]
                        }
                    ]
                }
            ]
        }
