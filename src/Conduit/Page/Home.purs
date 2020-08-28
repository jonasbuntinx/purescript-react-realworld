module Conduit.Page.Home (mkHomePage) where

import Prelude
import Conduit.Capability.Api (listArticles, listFeed, listTags, toggleFavorite)
import Conduit.Component.App as App
import Conduit.Component.ArticleList (articleList)
import Conduit.Component.Pagination (pagination)
import Conduit.Component.Tabs as Tabs
import Conduit.Data.Article (defaultArticlesQuery)
import Conduit.Hook.Auth (useAuth)
import Conduit.Page.Utils (_articles)
import Data.Foldable (for_, traverse_)
import Data.Lens (preview, set)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Monoid (guard)
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
    React.useEffect (isJust auth) do
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
      listTags >>= \res -> self.setState _ { tags = RemoteData.fromEither res }
    LoadArticles tab pagination -> do
      let
        query = defaultArticlesQuery { offset = Just pagination.offset, limit = Just pagination.limit }

        request = case tab of
          Feed -> listFeed query
          Global -> listArticles query
          Tag tag -> listArticles (query { tag = Just tag })
      self.setState _ { articles = RemoteData.Loading, tab = tab, pagination = pagination }
      request >>= \res -> self.setState _ { articles = RemoteData.fromEither res }
    ToggleFavorite ix -> for_ (preview (_articles ix) self.state) (toggleFavorite >=> traverse_ (self.setState <<< set (_articles ix)))

  render env auth store props =
    container (guard (isNothing auth) banner)
      [ mainView
      , R.div
          { className: "col-md-3"
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
        { className: "col-md-9"
        , children:
            [ Tabs.tabs
                { className: "feed-toggle"
                , selectedTab: Just store.state.tab
                , tabs:
                    [ { id: Feed
                      , label: R.text "Your Feed"
                      , disabled: isNothing auth
                      , content: tabContent
                      }
                    , { id: Global
                      , label: R.text "Global Feed"
                      , disabled: false
                      , content: tabContent
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
                              , content: tabContent
                              }
                            ]
                          _ -> []
                , onChange: \tab -> store.dispatch $ LoadArticles tab init.pagination
                }
            ]
        }

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
                  , onChange: store.dispatch <<< (LoadArticles store.state.tab)
                  , focusWindow: 3
                  , marginPages: 1
                  }
        ]

    renderTags = case store.state.tags of
      NotAsked -> R.div_ [ R.text "Tags not loaded" ]
      Loading -> R.div_ [ R.text "Loading Tags" ]
      Failure err -> R.div_ [ R.text $ "Failed loading tags" ]
      Success loadedTags -> R.div { className: "tag-list", children: map renderTag loadedTags }

    renderTag tag =
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
