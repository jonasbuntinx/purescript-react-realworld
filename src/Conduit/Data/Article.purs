module Conduit.Data.Article where

import Conduit.Data.Profile (Profile)
import Conduit.Data.Slug (Slug)
import Conduit.Data.Username (Username)
import Data.Maybe (Maybe(..))
import Foreign.Day (DateTime)

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
      , createdAt :: DateTime
      , favorited :: Boolean
      , favoritesCount :: Int
      , author :: Profile
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
