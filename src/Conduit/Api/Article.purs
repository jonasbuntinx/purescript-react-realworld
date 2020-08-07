module Conduit.Api.Article where

import Apiary.Media (JSON)
import Apiary.Route (GET, POST, PUT, DELETE)
import Apiary.Types (None)
import Conduit.Data.Article (Article, ArticleRep)
import Conduit.Data.Comment (Comment, CommentId)
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

type ListFeed
  = GET "/api/articles/feed"
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

type DeleteArticle
  = DELETE "/api/articles/:slug"
      { path ::
          { slug :: Slug
          }
      , response ::
          { ok :: None
          }
      }

-- | Favorite
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

-- | Comment
type ListComments
  = GET "/api/articles/:slug/comments"
      { path ::
          { slug :: Slug
          }
      , response ::
          { ok ::
              JSON
                { comments :: Array Comment
                }
          }
      }

type CreateComment
  = POST "/api/articles/:slug/comments"
      { path ::
          { slug :: Slug
          }
      , body ::
          JSON
            { comment ::
                { body :: String
                }
            }
      , response ::
          { ok ::
              JSON
                { comment :: Comment
                }
          }
      }

type DeleteComment
  = DELETE "/api/articles/:slug/comments/:id"
      { path ::
          { slug :: Slug
          , id :: CommentId
          }
      , response ::
          { ok :: None
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
