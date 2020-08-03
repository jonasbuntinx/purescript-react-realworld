module Conduit.Component.ArticleList where

import Prelude
import Apiary.Types as Apiary
import Conduit.Data.Article (Article)
import Conduit.Data.Avatar as Avatar
import Conduit.Data.Route (Route(..))
import Conduit.Data.Username as Username
import Conduit.Effects.Routing (navigate)
import Data.Array as Array
import Foreign.Moment (Format(..), format)
import Network.RemoteData (RemoteData(..))
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import React.Basic.Hooks as React

-- | Article List
type ArticleListProps
  = { articles :: RemoteData Apiary.Error (Array Article)
    }

articleList :: ArticleListProps -> React.JSX
articleList props =
  R.div
    { className: "article-preview"
    , children:
        case props.articles of
          NotAsked -> [ R.text "Articles not yet loaded" ]
          Loading -> [ R.text "Loading..." ]
          Failure _ -> [ R.text "Error loading articles " ]
          Success articles
            | Array.null articles -> [ R.text "No articles are here...yet!" ]
          Success articles -> articles <#> \article -> acticlePreview { article }
    }

-- | Article Preview
type ArticlePreviewProps
  = { article :: Article
    }

acticlePreview :: ArticlePreviewProps -> React.JSX
acticlePreview { article } =
  R.div
    { className: "article-preview"
    , children:
        [ R.div
            { className: "article-meta"
            , children:
                [ R.a
                    { href: "#"
                    , onClick: handler preventDefault $ const $ navigate $ Profile article.author.username
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
                        [ R.a
                            { className: "author"
                            , onClick: handler preventDefault $ const $ navigate $ Profile article.author.username
                            , children: [ R.text $ Username.toString article.author.username ]
                            }
                        , R.span
                            { className: "date"
                            , children: [ R.text $ format (Format "MMMM Do, YYYY") article.createdAt ]
                            }
                        ]
                    }
                , R.div
                    { className: "pull-xs-right"
                    , children: [ favoriteButton { size: Icon, article } ]
                    }
                ]
            }
        , R.a
            { className: "preview-link"
            , href: "#"
            , onClick: handler preventDefault $ const $ navigate Home
            , children:
                [ R.h1_
                    [ R.text article.title ]
                , R.p_
                    [ R.text article.description ]
                , R.span_
                    [ R.text "Read more..." ]
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

-- | Buttons
data ButtonSize
  = Icon
  | Medium

derive instance eqButtonSize :: Eq ButtonSize

type FavoriteButtonProps
  = { size :: ButtonSize
    , article :: Article
    }

favoriteButton :: FavoriteButtonProps -> React.JSX
favoriteButton { size, article } =
  R.button
    { className: "btn btn-sm " <> if article.favorited then "btn-primary" else "btn-outline-primary"
    , children:
        [ R.i
            { className: "ion-heart"
            , children: []
            }
        , R.span_
            [ R.text
                $ case article.favorited, size of
                    true, Medium -> " Unfavorite Article"
                    _, Medium -> " Favorite Article"
                    _, _ -> " "
            ]
        , R.span
            { className: "counter"
            , children:
                [ R.text
                    $ case size of
                        Icon -> " " <> show article.favoritesCount
                        _ -> " (" <> show article.favoritesCount <> ")"
                ]
            }
        ]
    }
