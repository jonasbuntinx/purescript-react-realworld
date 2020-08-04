module Conduit.Api.Article where

import Apiary.Media (JSON)
import Apiary.Route (GET, POST, PUT, DELETE)
import Conduit.Data.Article (Article, ArticleRep)
import Conduit.Data.Slug (Slug)
import Conduit.Data.Username (Username)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)

type ListArticles
  = GET "/api/articles"
      { query :: ArticlesQuery
      , response ::
          { ok ::
              JSON
                { articles :: Array Article
                , articlesCount :: Int
                }
          }
      }

type GetArticle
  = GET "/api/articles/:slug"
      { path ::
          { slug :: Slug
          }
      , response ::
          { ok ::
              JSON
                { article :: Article
                }
          , notFound ::
              JSON
                { status :: String
                , error :: String
                }
          }
      }

type CreateArticle
  = POST "/api/articles"
      { body ::
          JSON
            { article :: { | ArticleRep () }
            }
      , response ::
          { ok ::
              JSON
                { article :: Article
                }
          , unprocessableEntity ::
              JSON { errors :: Object (Array String) }
          }
      }

type UpdateArticle
  = PUT "/api/articles/:slug"
      { path ::
          { slug :: Slug
          }
      , body ::
          JSON
            { article :: { | ArticleRep () }
            }
      , response ::
          { ok ::
              JSON
                { article :: Article
                }
          , unprocessableEntity ::
              JSON { errors :: Object (Array String) }
          }
      }

type FavoriteArticle
  = POST "/api/articles/:slug/favorite"
      { path ::
          { slug :: Slug
          }
      , response ::
          { ok ::
              JSON
                { article :: Article
                }
          }
      }

type UnfavoriteArticle
  = DELETE "/api/articles/:slug/favorite"
      { path ::
          { slug :: Slug
          }
      , response ::
          { ok ::
              JSON
                { article :: Article
                }
          }
      }

-- | Types
type ArticlesQuery
  = { tag :: Maybe String
    , author :: Maybe Username
    , favorited :: Maybe Username
    , offset :: Maybe Int
    , limit :: Maybe Int
    }

defaultArticlesQuery :: ArticlesQuery
defaultArticlesQuery =
  { tag: Nothing
  , author: Nothing
  , favorited: Nothing
  , offset: Nothing
  , limit: Nothing
  }
