module Conduit.Page.Home (mkHomePage) where

import Prelude
import Conduit.Capability.Auth (readAuth, readAuthEvent)
import Conduit.Capability.Resource.Article (listArticles, listFeed, toggleFavorite)
import Conduit.Capability.Resource.Tag (listTags)
import Conduit.Capability.Routing (navigate)
import Conduit.Component.App as App
import Conduit.Component.ArticleList (articleList)
import Conduit.Component.Pagination (pagination)
import Conduit.Component.Tabs as Tabs
import Conduit.Data.Article (defaultArticlesQuery)
import Conduit.Data.Auth (Auth)
import Conduit.Data.Route (Route)
import Conduit.Page.Utils (_articles)
import Data.Foldable (for_, traverse_)
import Data.Lens (preview, set)
import Data.Maybe (Maybe(..), isNothing)
import Data.Monoid (guard)
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import React.Basic.Hooks as React
import React.Halo as Halo

data Tab
  = Feed
  | Global
  | Tag String

derive instance eqTab :: Eq Tab

data Action
  = Initialize
  | UpdateAuth (Maybe Auth)
  | Navigate Route
  | LoadTags
  | LoadArticles Tab { offset :: Int, limit :: Int }
  | ToggleFavorite Int

mkHomePage :: App.Component Unit
mkHomePage = App.component "HomePage" { initialState, eval, render }
  where
  initialState =
    { auth: Nothing
    , tags: NotAsked
    , articles: NotAsked
    , pagination: { offset: 0, limit: 10 }
    , tab: Global
    }

  eval =
    Halo.mkEval
      _
        { onInitialize = \_ -> Just Initialize
        , onAction = handleAction
        }

  handleAction = case _ of
    Initialize -> do
      auth <- readAuth
      handleAction $ UpdateAuth auth
      authEvent <- readAuthEvent
      void $ Halo.subscribe $ map UpdateAuth authEvent
      handleAction LoadTags
    UpdateAuth auth -> do
      state <- Halo.get
      Halo.modify_ _ { auth = auth }
      case auth of
        Nothing -> handleAction $ LoadArticles state.tab state.pagination
        Just _ -> handleAction $ LoadArticles Feed state.pagination
    Navigate route -> do
      navigate route
    LoadTags -> do
      Halo.modify_ _ { tags = RemoteData.Loading }
      response <- listTags
      Halo.modify_ _ { tags = RemoteData.fromEither response }
    LoadArticles tab pagination -> do
      Halo.modify_ _ { articles = RemoteData.Loading, tab = tab, pagination = pagination }
      let
        query = defaultArticlesQuery { offset = Just pagination.offset, limit = Just pagination.limit }
      response <- case tab of
        Feed -> listFeed query
        Global -> listArticles query
        Tag tag -> listArticles (query { tag = Just tag })
      Halo.modify_ _ { articles = RemoteData.fromEither response }
    ToggleFavorite ix -> do
      state <- Halo.get
      for_ (preview (_articles ix) state) (toggleFavorite >=> traverse_ (Halo.modify_ <<< set (_articles ix)))

  render { state, send } =
    container (guard (isNothing state.auth) banner)
      [ mainView
      , R.div
          { className: "col-md-3 col-xs-12"
          , children:
              [ R.div
                  { className: "sidebar"
                  , children:
                      [ R.p_ [ R.text "Popular Tags" ]
                      , renderTags
                      ]
                  }
              ]
          }
      ]
    where
    mainView =
      R.div
        { className: "col-md-9 col-xs-12"
        , children:
            [ Tabs.tabs
                { className: "feed-toggle"
                , selectedTab: Just state.tab
                , tabs:
                    [ { id: Feed
                      , label: R.text "Your Feed"
                      , disabled: isNothing state.auth
                      , content: tabContent
                      }
                    , { id: Global
                      , label: R.text "Global Feed"
                      , disabled: false
                      , content: tabContent
                      }
                    ]
                      <> case state.tab of
                          Tag tag ->
                            [ { id: Tag tag
                              , label:
                                  React.fragment
                                    [ R.i
                                        { className: "ion-pound"
                                        , children: []
                                        }
                                    , R.text $ " " <> tag
                                    ]
                              , disabled: false
                              , content: tabContent
                              }
                            ]
                          _ -> []
                , onChange: \tab -> send $ LoadArticles tab initialState.pagination
                }
            ]
        }

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
                  , onChange: send <<< (LoadArticles state.tab)
                  , focusWindow: 3
                  , marginPages: 1
                  }
        ]

    renderTags = case state.tags of
      NotAsked -> R.div_ [ R.text "Tags not loaded" ]
      Loading -> R.div_ [ R.text "Loading Tags" ]
      Failure err -> R.div_ [ R.text $ "Failed loading tags" ]
      Success loadedTags -> R.div { className: "tag-list", children: map renderTag loadedTags }

    renderTag tag =
      R.a
        { className: "tag-default tag-pill"
        , href: "#"
        , onClick: handler preventDefault $ const $ send $ LoadArticles (Tag tag) state.pagination
        , children: [ R.text tag ]
        }

    banner =
      R.div
        { className: "banner"
        , children:
            [ R.div
                { className: "container"
                , children:
                    [ R.h1
                        { className: "logo-font"
                        , children: [ R.text "conduit" ]
                        }
                    , R.p_
                        [ R.text "A place to share your knowledge." ]
                    ]
                }
            ]
        }

    container header children =
      R.div
        { className: "home-page"
        , children:
            [ header
            , R.div
                { className: "container page"
                , children:
                    [ R.div
                        { className: "row"
                        , children
                        }
                    ]
                }
            ]
        }
