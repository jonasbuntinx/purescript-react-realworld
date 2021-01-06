module Conduit.Page.Home (makeHomePage) where

import Prelude
import Conduit.Capability.Api (listArticles, listFeed, listTags, toggleFavorite)
import Conduit.Component.ArticleList (articleList)
import Conduit.Component.Page as Page
import Conduit.Component.Pagination (pagination)
import Conduit.Component.Tabs as Tabs
import Conduit.Data.Article (defaultArticlesQuery)
import Conduit.Hook.Auth (useAuth)
import Conduit.Page.Utils (_articles)
import Control.Monad.State (modify_)
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
import React.Halo as Halo

data Tab
  = Feed
  | Global
  | Tag String

derive instance eqTab :: Eq Tab

data Action
  = LoadTags
  | LoadArticles Tab { offset :: Int, limit :: Int }
  | ToggleFavorite Int

makeHomePage :: Page.Component Unit
makeHomePage =
  Page.component' "HomePage" { initialState, eval } \self -> React.do
    auth <- useAuth self.env
    React.useEffect (isJust auth) do
      case auth of
        Nothing -> self.send $ LoadArticles self.state.tab self.state.pagination
        Just _ -> self.send $ LoadArticles Feed self.state.pagination
      mempty
    pure $ render auth self
  where
  initialState =
    { tags: NotAsked
    , articles: NotAsked
    , pagination: { offset: 0, limit: 10 }
    , tab: Global
    }

  eval =
    Halo.makeEval
      _
        { onInitialize = \_ -> Just $ LoadTags
        , onAction = handleAction
        }

  handleAction = case _ of
    LoadTags -> do
      modify_ _ { tags = RemoteData.Loading }
      response <- listTags
      modify_ _ { tags = RemoteData.fromEither response }
    LoadArticles tab pagination -> do
      modify_ _ { articles = RemoteData.Loading, tab = tab, pagination = pagination }
      let
        query = defaultArticlesQuery { offset = Just pagination.offset, limit = Just pagination.limit }
      response <- case tab of
        Feed -> listFeed query
        Global -> listArticles query
        Tag tag -> listArticles (query { tag = Just tag })
      modify_ _ { articles = RemoteData.fromEither response }
    ToggleFavorite ix -> do
      state <- Halo.get
      for_ (preview (_articles ix) state) (toggleFavorite >=> traverse_ (modify_ <<< set (_articles ix)))

  render auth { env, props, state, send } =
    container (guard (isNothing auth) banner)
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
                      , disabled: isNothing auth
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
            , onNavigate: env.router.navigate
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
