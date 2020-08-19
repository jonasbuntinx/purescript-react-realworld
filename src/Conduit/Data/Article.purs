module Conduit.Data.Article where

import Conduit.Data.PreciseDateTime (PreciseDateTime)
import Conduit.Data.Profile (Author)
import Conduit.Data.Slug (Slug)
import Conduit.Data.Username (Username)
import Data.Maybe (Maybe(..))

type ArticleRep r
  = ( title :: String
    , description :: String
    , body :: String
    , tagList :: Array String
    | r
    )

type Article
  = {
    | ArticleRep
      ( slug :: Slug
      , createdAt :: PreciseDateTime
      , favorited :: Boolean
      , favoritesCount :: Int
      , author :: Author
      )
    }

type ArticlesQuery
  = { tag :: Maybe String
    , author :: Maybe Username
    , favorited :: Maybe Username
    , offset :: Maybe Int
    , limit :: Maybe Int
    }

-- | Helpers
defaultArticlesQuery :: ArticlesQuery
defaultArticlesQuery =
  { tag: Nothing
  , author: Nothing
  , favorited: Nothing
  , offset: Nothing
  , limit: Nothing
  }
