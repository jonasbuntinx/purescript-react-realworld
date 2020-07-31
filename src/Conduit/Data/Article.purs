module Conduit.Data.Article where

import Conduit.Data.Profile (Author)
import Conduit.Data.Slug (Slug)
import Foreign.Moment (Moment)

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
      , createdAt :: Moment
      , favorited :: Boolean
      , favoritesCount :: Int
      , author :: Author
      )
    }
