module Conduit.Api.Article where

import Apiary.Media (JSON)
import Apiary.Route (GET, PUT, POST)
import Conduit.Data.Article (Article, ArticleRep)
import Conduit.Data.Slug (Slug)
import Foreign.Object (Object)

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
