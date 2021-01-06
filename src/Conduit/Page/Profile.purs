module Conduit.Page.Profile (Props, Tab(..), makeProfilePage) where

import Prelude
import Conduit.Capability.Api (getProfile, listArticles, toggleFavorite, toggleFollow)
import Conduit.Capability.Routing (redirect)
import Conduit.Component.ArticleList (articleList)
import Conduit.Component.Buttons (followButton)
import Conduit.Component.Page as Page
import Conduit.Component.Pagination (pagination)
import Conduit.Component.Tabs as Tabs
import Conduit.Data.Article (defaultArticlesQuery)
import Conduit.Data.Avatar as Avatar
import Conduit.Data.Error (Error(..))
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username (Username)
import Conduit.Data.Username as Username
import Conduit.Hook.Auth (useAuth)
import Conduit.Page.Utils (_articles, _profile)
import Control.Monad.State (modify_)
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
  = LoadProfile
  | LoadArticles { offset :: Int, limit :: Int }
  | ToggleFavorite Int
  | ToggleFollow

makeProfilePage :: Page.Component Props
makeProfilePage =
  Page.component' "ProfilePage" { initialState, eval } \self -> React.do
    auth <- useAuth self.env
    pure $ render auth self
  where
  initialState =
    { selectedTab: Nothing
    , profile: RemoteData.NotAsked
    , articles: RemoteData.NotAsked
    , pagination: { offset: 0, limit: 5 }
    }

  eval = case _ of
    Halo.Initialize _ -> do
      eval $ Halo.Action LoadProfile
      eval $ Halo.Action $ LoadArticles initialState.pagination
    Halo.Update prev next -> do
      guard (prev.username /= next.username) do
        void $ Halo.fork $ eval $ Halo.Action LoadProfile
      guard (prev.username /= next.username || prev.tab /= next.tab) do
        void $ Halo.fork $ eval $ Halo.Action $ LoadArticles initialState.pagination
    Halo.Action LoadProfile -> do
      props <- Halo.props
      modify_ _ { profile = RemoteData.Loading }
      bind (getProfile props.username) case _ of
        Left (NotFound _) -> redirect Home
        Left error -> modify_ _ { profile = RemoteData.Failure error }
        Right profile -> modify_ _ { profile = RemoteData.Success profile }
    Halo.Action (LoadArticles pagination) -> do
      props <- Halo.props
      let
        query = defaultArticlesQuery { offset = Just pagination.offset, limit = Just pagination.limit }

        request =
          listArticles case props.tab of
            Published -> query { author = Just props.username }
            Favorited -> query { favorited = Just props.username }
      modify_ _ { articles = RemoteData.Loading, pagination = pagination }
      request >>= \res -> modify_ _ { articles = RemoteData.fromEither res }
    Halo.Action (ToggleFavorite ix) -> do
      state <- Halo.get
      for_ (preview (_articles ix) state) (toggleFavorite >=> traverse_ (modify_ <<< set (_articles ix)))
    Halo.Action ToggleFollow -> do
      state <- Halo.get
      for_ (preview _profile state) (toggleFollow >=> traverse_ (modify_ <<< set _profile))
    Halo.Finalize -> pure unit

  render auth { env, props, state, send } =
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
                Published -> env.router.navigate $ Profile props.username
                Favorited -> env.router.navigate $ Favorites props.username
          }
      ]
    where
    tabContent =
      R.div_
        [ articleList
            { articles: state.articles <#> _.articles
            , onNavigate: env.router.navigate
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
                                    , if (Just props.username == map _.username auth) then
                                        R.button
                                          { className: "btn btn-sm action-btn btn-outline-secondary"
                                          , onClick: handler_ $ env.router.navigate Settings
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
