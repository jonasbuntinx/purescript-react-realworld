module Conduit.Page.Profile (Props, Tab(..), mkProfilePage) where

import Prelude
import Apiary.Route (Route(..)) as Apiary
import Apiary.Types (none) as Apiary
import Conduit.Api.Endpoints (GetProfile, ListArticles)
import Conduit.Api.Utils as Utils
import Conduit.Capability.Routing (navigate)
import Conduit.Component.App as App
import Conduit.Component.ArticleList (articleList)
import Conduit.Component.Buttons (followButton)
import Conduit.Component.Pagination (pagination)
import Conduit.Component.Tabs as Tabs
import Conduit.Data.Article (defaultArticlesQuery)
import Conduit.Data.Avatar as Avatar
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username (Username)
import Conduit.Data.Username as Username
import Conduit.Hook.Auth (useAuth)
import Conduit.Page.Utils (_articles, _profile, toggleFavorite, toggleFollow)
import Data.Either (Either(..), either)
import Data.Lens (preview, set)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Tuple.Nested ((/\))
import Data.Variant as Variant
import Network.RemoteData as RemoteData
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks as React

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
  | LoadArticles { offset :: Int, limit :: Int }
  | ToggleFavorite Int
  | ToggleFollow

mkProfilePage :: App.Component Props
mkProfilePage =
  App.component "ProfilePage" { init, update } \env store props -> React.do
    auth <- useAuth env
    React.useEffect props.username do
      store.dispatch Initialize
      mempty
    React.useEffect (props.username /\ props.tab) do
      store.dispatch $ LoadArticles init.pagination
      mempty
    pure $ render env auth store props
  where
  init =
    { selectedTab: Nothing
    , profile: RemoteData.NotAsked
    , articles: RemoteData.NotAsked
    , pagination: { offset: 0, limit: 5 }
    }

  update self = case _ of
    Initialize -> do
      self.setState _ { profile = RemoteData.Loading }
      res <- Utils.makeRequest (Apiary.Route :: GetProfile) { username: self.props.username } Apiary.none Apiary.none
      case res of
        Left error -> self.setState _ { profile = RemoteData.Failure error }
        Right response ->
          response
            # Variant.match
                { ok: \{ profile } -> self.setState _ { profile = RemoteData.Success profile }
                , notFound: \_ -> navigate Home
                }
    LoadArticles pagination -> do
      let
        query =
          defaultArticlesQuery { offset = Just pagination.offset, limit = Just pagination.limit }
            # case self.props.tab of
                Published -> _ { author = Just self.props.username }
                Favorited -> _ { favorited = Just self.props.username }
      self.setState _ { articles = RemoteData.Loading, pagination = pagination }
      res <- Utils.makeRequest (Apiary.Route :: ListArticles) Apiary.none query Apiary.none
      self.setState _ { articles = res # either RemoteData.Failure (Variant.match { ok: RemoteData.Success }) }
    ToggleFavorite ix -> toggleFavorite (preview (_articles ix) self.state) (self.setState <<< set (_articles ix))
    ToggleFollow -> toggleFollow (preview _profile self.state) (self.setState <<< set _profile)

  render env auth store props =
    guard (not $ RemoteData.isLoading store.state.profile) container userInfo
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
                Published -> env.routing.navigate $ Profile props.username
                Favorited -> env.routing.navigate $ Favorites props.username
          }
      ]
    where
    tabContent =
      R.div_
        [ articleList
            { articles: store.state.articles <#> _.articles
            , onNavigate: env.routing.navigate
            , onFavoriteToggle: store.dispatch <<< ToggleFavorite
            }
        , store.state.articles
            # RemoteData.maybe React.empty \{ articlesCount } ->
                pagination
                  { offset: store.state.pagination.offset
                  , limit: store.state.pagination.limit
                  , totalCount: articlesCount
                  , onChange: store.dispatch <<< LoadArticles
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
                                        , src: Avatar.toString $ RemoteData.maybe Avatar.blank (Avatar.withDefault <<< _.image) store.state.profile
                                        }
                                    , R.h4_ [ R.text $ Username.toString props.username ]
                                    , maybe React.empty (\bio -> R.p_ [ R.text bio ]) (RemoteData.toMaybe store.state.profile >>= _.bio)
                                    , if (Just props.username == map _.username auth) then
                                        R.button
                                          { className: "btn btn-sm action-btn btn-outline-secondary"
                                          , onClick: handler_ $ env.routing.navigate Settings
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
                                          { following: RemoteData.maybe false _.following store.state.profile
                                          , username: props.username
                                          , onClick: handler_ $ store.dispatch ToggleFollow
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
