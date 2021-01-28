module Conduit.Page.Profile (Props, Tab(..), mkProfilePage) where

import Prelude
import Conduit.Capability.Auth (readAuth, readAuthEvent)
import Conduit.Capability.Resource.Article (listArticles, toggleFavorite)
import Conduit.Capability.Resource.Profile (getProfile, toggleFollow)
import Conduit.Capability.Routing (navigate, redirect)
import Conduit.Component.App as App
import Conduit.Component.ArticleList (articleList)
import Conduit.Component.Buttons (followButton)
import Conduit.Component.Pagination (pagination)
import Conduit.Component.Tabs as Tabs
import Conduit.Data.Article (defaultArticlesQuery)
import Conduit.Data.Auth (Auth)
import Conduit.Data.Avatar as Avatar
import Conduit.Data.Error (Error(..))
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username (Username)
import Conduit.Data.Username as Username
import Conduit.Page.Utils (_articles, _profile)
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Lens (preview, set)
import Data.Maybe (Maybe(..), maybe)
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

derive instance eqTab :: Eq Tab

data Action
  = Initialize
  | LoadResources { profile :: Boolean, articles :: Boolean }
  | UpdateAuth (Maybe Auth)
  | Navigate Route
  | LoadProfile
  | LoadArticles { offset :: Int, limit :: Int }
  | ToggleFavorite Int
  | ToggleFollow

mkProfilePage :: App.Component Props
mkProfilePage = App.component "ProfilePage" { initialState, eval, render }
  where
  initialState =
    { auth: Nothing
    , selectedTab: Nothing
    , profile: RemoteData.NotAsked
    , articles: RemoteData.NotAsked
    , pagination: { offset: 0, limit: 5 }
    }

  eval =
    Halo.mkEval
      _
        { onInitialize = \_ -> Just Initialize
        , onUpdate =
          \prev next -> Just $ LoadResources { profile: prev.username /= next.username, articles: prev.username /= next.username || prev.tab /= next.tab }
        , onAction = handleAction
        }

  handleAction = case _ of
    Initialize -> do
      auth <- readAuth
      handleAction $ UpdateAuth auth
      authEvent <- readAuthEvent
      void $ Halo.subscribe $ map UpdateAuth authEvent
    LoadResources { profile, articles } -> do
      when profile do
        void $ Halo.fork $ handleAction LoadProfile
      when articles do
        void $ Halo.fork $ handleAction $ LoadArticles initialState.pagination
    UpdateAuth auth -> Halo.modify_ _ { auth = auth }
    Navigate route -> navigate route
    LoadProfile -> do
      props <- Halo.props
      Halo.modify_ _ { profile = RemoteData.Loading }
      response <- getProfile props.username
      case response of
        Left (NotFound _) -> redirect Home
        Left error -> Halo.modify_ _ { profile = RemoteData.Failure error }
        Right profile -> Halo.modify_ _ { profile = RemoteData.Success profile }
    LoadArticles pagination -> do
      props <- Halo.props
      let
        query = defaultArticlesQuery { offset = Just pagination.offset, limit = Just pagination.limit }
      Halo.modify_ _ { articles = RemoteData.Loading, pagination = pagination }
      response <-
        listArticles case props.tab of
          Published -> query { author = Just props.username }
          Favorited -> query { favorited = Just props.username }
      Halo.modify_ _ { articles = RemoteData.fromEither response }
    ToggleFavorite ix -> do
      state <- Halo.get
      for_ (preview (_articles ix) state) (toggleFavorite >=> traverse_ (Halo.modify_ <<< set (_articles ix)))
    ToggleFollow -> do
      state <- Halo.get
      for_ (preview _profile state) (toggleFollow >=> traverse_ (Halo.modify_ <<< set _profile))

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
                                          { following: RemoteData.maybe false _.following state.profile
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
