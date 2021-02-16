module Conduit.Page.Profile (Props, Tab(..), mkInitialState, mkComponent) where

import Prelude
import Conduit.AppM (AppM)
import Conduit.Capability.Access (readAccess, readAccessEvent)
import Conduit.Capability.Resource.Article (listArticles, toggleFavorite)
import Conduit.Capability.Resource.Profile (getProfile, toggleFollow)
import Conduit.Capability.Routing (navigate, redirect)
import Conduit.Component.App as App
import Conduit.Component.ArticleList (articleList)
import Conduit.Component.Buttons (followButton)
import Conduit.Component.Pagination (pagination)
import Conduit.Component.Tabs as Tabs
import Conduit.Data.Access (Access(..))
import Conduit.Data.Access as Access
import Conduit.Data.Article (Article, defaultArticlesQuery)
import Conduit.Data.Auth (Auth)
import Conduit.Data.Avatar as Avatar
import Conduit.Data.Error (Error(..))
import Conduit.Data.Profile (Profile)
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username (Username)
import Conduit.Data.Username as Username
import Conduit.Page.Utils (_articles, _profile)
import Control.Parallel (parTraverse_)
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Lens (preview, set)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Monoid (guard)
import Network.RemoteData as RemoteData
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks as React
import React.Halo as Halo

-- | Props
type Props
  = { username :: Username
    , tab :: Tab
    }

data Tab
  = Published
  | Favorited

derive instance eqTab :: Eq Tab

-- | State
type State
  = { access :: Access Auth
    , selectedTab :: Maybe Tab
    , profile :: RemoteData.RemoteData Error Profile
    , articles :: RemoteData.RemoteData Error { articles :: Array Article, articlesCount :: Int }
    , pagination :: { offset :: Int, limit :: Int }
    }

emptyState :: State
emptyState =
  { access: Public
  , selectedTab: Nothing
  , profile: RemoteData.NotAsked
  , articles: RemoteData.NotAsked
  , pagination: { offset: 0, limit: 5 }
  }

mkInitialState :: Props -> AppM State
mkInitialState { username, tab } = do
  profile <- getProfile username
  articles <-
    listArticles case tab of
      Published -> defaultArticlesQuery { author = Just username }
      Favorited -> defaultArticlesQuery { favorited = Just username }
  pure
    $ emptyState
        { profile = RemoteData.fromEither profile
        , articles = RemoteData.fromEither articles
        }

-- | Component
data Action
  = Initialize
  | OnPropsUpdate Props Props
  | UpdateAccess (Access Auth)
  | Navigate Route
  | LoadProfile
  | LoadArticles { offset :: Int, limit :: Int }
  | ToggleFavorite Int
  | ToggleFollow

mkComponent :: Maybe State -> App.Component Props
mkComponent maybeInitialState = App.component "ProfilePage" { initialState, eval, render }
  where
  initialState = fromMaybe emptyState maybeInitialState

  eval =
    Halo.mkEval
      _
        { onInitialize = \_ -> Just Initialize
        , onUpdate = \prev next -> Just $ OnPropsUpdate prev next
        , onAction = handleAction
        }

  handleAction = case _ of
    Initialize -> do
      access <- readAccess
      handleAction $ UpdateAccess access
      accessEvent <- readAccessEvent
      void $ Halo.subscribe $ map UpdateAccess accessEvent
      guard (isNothing maybeInitialState) do
        parTraverse_ handleAction
          [ LoadProfile
          , LoadArticles initialState.pagination
          ]
    OnPropsUpdate prev next -> do
      let
        reloadProfile = prev.username /= next.username

        reloadArticles = reloadProfile || prev.tab /= next.tab

        actions =
          join
            [ guard reloadProfile [ LoadProfile ]
            , guard reloadArticles [ LoadArticles initialState.pagination ]
            ]
      parTraverse_ handleAction actions
    UpdateAccess access -> Halo.modify_ _ { access = access }
    Navigate route -> navigate route
    LoadProfile -> do
      props <- Halo.props
      Halo.modify_ _ { profile = RemoteData.Loading }
      response <- getProfile props.username
      Halo.modify_ _ { profile = RemoteData.fromEither response }
      case response of
        Left (NotFound _) -> redirect Home
        _ -> pure unit
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
                                    , if (Just props.username == map _.username (Access.toMaybe state.access)) then
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
