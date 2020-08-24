module Conduit.Page.Home (mkHomePage) where

import Prelude
import Apiary.Route (Route(..)) as Apiary
import Apiary.Types (none) as Apiary
import Conduit.Api.Endpoints (ListArticles, ListFeed, ListTags)
import Conduit.Api.Utils as Utils
import Conduit.AppM (runAppM)
import Conduit.Capability.Routing (navigate)
import Conduit.Component.App as App
import Conduit.Component.ArticleList (articleList)
import Conduit.Component.Pagination (pagination)
import Conduit.Component.Tabs as Tabs
import Conduit.Data.Article (defaultArticlesQuery)
import Conduit.Hook.Auth (useAuth)
import Conduit.Page.Utils (_articles, toggleFavorite)
import Data.Either (either)
import Data.Lens (preview, set)
import Data.Maybe (Maybe(..), isNothing)
import Data.Monoid (guard)
import Data.Variant as Variant
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import React.Basic.Hooks as React

data Tab
  = Feed
  | Global
  | Tag String

derive instance eqTab :: Eq Tab

data Action
  = LoadTags
  | LoadArticles Tab { offset :: Int, limit :: Int }
  | ToggleFavorite Int

mkHomePage :: App.Component Unit
mkHomePage =
  App.component "HomePage" { init, update } \env store props -> React.do
    auth <- useAuth env
    React.useEffect auth do
      case auth of
        Nothing -> store.dispatch $ LoadArticles store.state.tab store.state.pagination
        Just _ -> store.dispatch $ LoadArticles Feed store.state.pagination
      mempty
    React.useEffectOnce do
      store.dispatch LoadTags
      mempty
    pure $ render env auth store props
  where
  init =
    { tags: NotAsked
    , articles: NotAsked
    , pagination: { offset: 0, limit: 10 }
    , tab: Global
    }

  update self = case _ of
    LoadTags -> do
      self.setState _ { tags = RemoteData.Loading }
      res <- Utils.makeRequest (Apiary.Route :: ListTags) Apiary.none Apiary.none Apiary.none
      self.setState _ { tags = res # either RemoteData.Failure (Variant.match { ok: RemoteData.Success <<< _.tags }) }
    LoadArticles tab pagination -> do
      let
        query = defaultArticlesQuery { offset = Just pagination.offset, limit = Just pagination.limit }
      self.setState _ { articles = RemoteData.Loading, tab = tab, pagination = pagination }
      res <- case tab of
        Feed -> Utils.makeSecureRequest (Apiary.Route :: ListFeed) Apiary.none query Apiary.none
        Global -> Utils.makeRequest (Apiary.Route :: ListArticles) Apiary.none query Apiary.none
        Tag tag -> Utils.makeRequest (Apiary.Route :: ListArticles) Apiary.none (query { tag = Just tag }) Apiary.none
      self.setState _ { articles = res # either RemoteData.Failure (Variant.match { ok: RemoteData.Success }) }
    ToggleFavorite ix -> toggleFavorite (preview (_articles ix) self.state) (self.setState <<< set (_articles ix))

  render env auth store props =
    container (guard (isNothing auth) banner)
      [ mainView env auth store
      , R.div
          { className: "col-md-3"
          , children:
              [ R.div
                  { className: "sidebar"
                  , children:
                      [ R.p_ [ R.text "Popular Tags" ]
                      , renderTags store
                      ]
                  }
              ]
          }
      ]

  mainView env auth store =
    R.div
      { className: "col-md-9"
      , children:
          [ Tabs.tabs
              { className: "feed-toggle"
              , selectedTab: Just store.state.tab
              , tabs:
                  [ { id: Feed
                    , label: R.text "Your Feed"
                    , disabled: isNothing auth
                    , content: tabContent env store
                    }
                  , { id: Global
                    , label: R.text "Global Feed"
                    , disabled: false
                    , content: tabContent env store
                    }
                  ]
                    <> case store.state.tab of
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
                            , content: tabContent env store
                            }
                          ]
                        _ -> []
              , onChange: \tab -> store.dispatch $ LoadArticles tab store.state.pagination
              }
          ]
      }

  tabContent env store =
    R.div_
      [ articleList
          { articles: store.state.articles <#> _.articles
          , onNavigate: runAppM env <<< navigate
          , onFavoriteToggle: store.dispatch <<< ToggleFavorite
          }
      , store.state.articles
          # RemoteData.maybe React.empty \{ articlesCount } ->
              pagination
                { offset: store.state.pagination.offset
                , limit: store.state.pagination.limit
                , totalCount: articlesCount
                , onChange: store.dispatch <<< (LoadArticles store.state.tab)
                , focusWindow: 3
                , marginPages: 1
                }
      ]

  renderTags store = case store.state.tags of
    NotAsked -> R.div_ [ R.text "Tags not loaded" ]
    Loading -> R.div_ [ R.text "Loading Tags" ]
    Failure err -> R.div_ [ R.text $ "Failed loading tags" ]
    Success loadedTags -> R.div { className: "tag-list", children: map (renderTag store) loadedTags }

  renderTag store tag =
    R.a
      { className: "tag-default tag-pill"
      , href: "#"
      , onClick: handler preventDefault $ const $ store.dispatch $ LoadArticles (Tag tag) store.state.pagination
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
