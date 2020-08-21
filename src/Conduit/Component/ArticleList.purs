module Conduit.Component.ArticleList where

import Prelude
import Conduit.Capability.Routing (toRouteURL)
import Conduit.Component.Buttons (ButtonSize(..), favoriteButton)
import Conduit.Component.Link as Link
import Conduit.Data.Article (Article)
import Conduit.Data.Avatar as Avatar
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username as Username
import Data.Array as Array
import Effect (Effect)
import Foreign.Day (toDisplay)
import Network.RemoteData (RemoteData(..))
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks as React

-- | Article List
type Props err
  = { articles :: RemoteData err (Array Article)
    , onNavigate :: Route -> Effect Unit
    , onFavoriteToggle :: Int -> Effect Unit
    }

articleList :: forall err. Props err -> React.JSX
articleList props = case props.articles of
  NotAsked ->
    R.div
      { className: "article-preview"
      , children: [ R.text "Articles not yet loaded" ]
      }
  Loading ->
    R.div
      { className: "article-preview"
      , children: [ R.text "Loading..." ]
      }
  Failure _ ->
    R.div
      { className: "article-preview"
      , children: [ R.text "Error loading articles " ]
      }
  Success articles
    | Array.null articles ->
      R.div
        { className: "article-preview"
        , children: [ R.text "No articles are here...yet!" ]
        }
  Success articles -> React.fragment $ Array.mapWithIndex preview articles
  where
  preview ix article =
    R.div
      { className: "article-preview"
      , children:
          [ R.div
              { className: "article-meta"
              , children:
                  [ Link.link
                      { className: ""
                      , href: toRouteURL $ Profile article.author.username
                      , onClick: props.onNavigate $ Profile article.author.username
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
                              , href: toRouteURL $ Profile article.author.username
                              , onClick: props.onNavigate $ Profile article.author.username
                              , children: [ R.text $ Username.toString article.author.username ]
                              }
                          , R.span
                              { className: "date"
                              , children: [ R.text $ toDisplay article.createdAt ]
                              }
                          ]
                      }
                  , R.div
                      { className: "pull-xs-right"
                      , children:
                          [ favoriteButton
                              { size: Icon
                              , favorited: article.favorited
                              , count: article.favoritesCount
                              , onClick: handler_ $ props.onFavoriteToggle ix
                              }
                          ]
                      }
                  ]
              }
          , Link.link
              { className: "preview-link"
              , href: toRouteURL $ ViewArticle article.slug
              , onClick: props.onNavigate $ ViewArticle article.slug
              , children:
                  [ R.h1_ [ R.text article.title ]
                  , R.p_ [ R.text article.description ]
                  , R.span_ [ R.text "Read more..." ]
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
