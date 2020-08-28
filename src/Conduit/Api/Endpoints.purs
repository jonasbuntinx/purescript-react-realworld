module Conduit.Api.Endpoints where

import Apiary.Media (JSON)
import Apiary.Route (GET, POST, PUT, DELETE)
import Apiary.Types (None)
import Conduit.Data.Article (Article, ArticleRep, ArticlesQuery)
import Conduit.Data.Comment (Comment, CommentId)
import Conduit.Data.Profile (ProfileRep, User, Profile)
import Conduit.Data.Slug (Slug)
import Conduit.Data.Username (Username)
import Foreign.Object (Object)

-- | User
type LoginUser
  = POST "/api/users/login"
      { body :: JSON { user :: { email :: String, password :: String } }
      , response ::
          { ok :: JSON { user :: User }
          , unprocessableEntity :: JSON { errors :: Object (Array String) }
          }
      }

type RegisterUser
  = POST "/api/users"
      { body :: JSON { user :: { username :: Username, email :: String, password :: String } }
      , response ::
          { ok :: JSON { user :: User }
          , unprocessableEntity :: JSON { errors :: Object (Array String) }
          }
      }

type GetUser
  = GET "/api/user"
      { response ::
          { ok :: JSON { user :: User }
          }
      }

type UpdateUser
  = PUT "/api/user"
      { body :: JSON { user :: { | ProfileRep ( email :: String, password :: String ) } }
      , response ::
          { ok :: JSON { user :: User }
          , unprocessableEntity :: JSON { errors :: Object (Array String) }
          }
      }

-- | Article
type ListArticles
  = GET "/api/articles"
      { query :: ArticlesQuery
      , response ::
          { ok :: JSON { articles :: Array Article, articlesCount :: Int }
          }
      }

type ListFeed
  = GET "/api/articles/feed"
      { query :: ArticlesQuery
      , response ::
          { ok :: JSON { articles :: Array Article, articlesCount :: Int }
          }
      }

type GetArticle
  = GET "/api/articles/:slug"
      { path :: { slug :: Slug }
      , response ::
          { ok :: JSON { article :: Article }
          , notFound :: JSON { status :: String, error :: String }
          }
      }

type CreateArticle
  = POST "/api/articles"
      { body :: JSON { article :: { | ArticleRep () } }
      , response ::
          { ok :: JSON { article :: Article }
          , unprocessableEntity :: JSON { errors :: Object (Array String) }
          }
      }

type UpdateArticle
  = PUT "/api/articles/:slug"
      { path :: { slug :: Slug }
      , body :: JSON { article :: { | ArticleRep () } }
      , response ::
          { ok :: JSON { article :: Article }
          , unprocessableEntity :: JSON { errors :: Object (Array String) }
          }
      }

type DeleteArticle
  = DELETE "/api/articles/:slug"
      { path :: { slug :: Slug }
      , response ::
          { ok :: None
          }
      }

-- | Favorite
type FavoriteArticle
  = POST "/api/articles/:slug/favorite"
      { path :: { slug :: Slug }
      , response ::
          { ok :: JSON { article :: Article }
          }
      }

type UnfavoriteArticle
  = DELETE "/api/articles/:slug/favorite"
      { path :: { slug :: Slug }
      , response ::
          { ok :: JSON { article :: Article }
          }
      }

-- | Comment
type ListComments
  = GET "/api/articles/:slug/comments"
      { path :: { slug :: Slug }
      , response ::
          { ok :: JSON { comments :: Array Comment }
          }
      }

type CreateComment
  = POST "/api/articles/:slug/comments"
      { path :: { slug :: Slug }
      , body :: JSON { comment :: { body :: String } }
      , response ::
          { ok :: JSON { comment :: Comment }
          }
      }

type DeleteComment
  = DELETE "/api/articles/:slug/comments/:id"
      { path :: { slug :: Slug, id :: CommentId }
      , response ::
          { ok :: None
          }
      }

-- | Profile
type GetProfile
  = GET "/api/profiles/:username"
      { path :: { username :: Username }
      , response ::
          { ok :: JSON { profile :: Profile }
          , notFound :: JSON { status :: String, error :: String }
          }
      }

-- | Follow
type FollowProfile
  = POST "/api/profiles/:username/follow"
      { path :: { username :: Username }
      , response ::
          { ok :: JSON { profile :: Profile }
          }
      }

type UnfollowProfile
  = DELETE "/api/profiles/:username/follow"
      { path :: { username :: Username }
      , response ::
          { ok :: JSON { profile :: Profile }
          }
      }

-- | Tag
type ListTags
  = GET "/api/tags"
      { response ::
          { ok :: JSON { tags :: Array String }
          }
      }
