module Conduit.Data.Article where

import Conduit.Data.Profile (Profile, profileCodec)
import Conduit.Data.Slug (Slug, slugCodec)
import Conduit.Data.Username (Username)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe(..))
import Foreign.Day (DateTime, dateTimeCodec)

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

-- | Codecs
articleCodec :: JsonCodec Article
articleCodec =
  CAR.object "Article"
    { title: CA.string
    , description: CA.string
    , body: CA.string
    , tagList: CA.array CA.string
    , slug: slugCodec
    , createdAt: dateTimeCodec
    , favorited: CA.boolean
    , favoritesCount: CA.int
    , author: profileCodec
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
