module Conduit.Data.Comment where

import Prelude

import Conduit.Data.Profile (Profile, profileCodec)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)
import Foreign.Day (DateTime, dateTimeCodec)

type Comment =
  { id :: CommentId
  , createdAt :: DateTime
  , updatedAt :: DateTime
  , body :: String
  , author :: Profile
  }

newtype CommentId
  = CommentId Int

derive instance Newtype CommentId _

derive newtype instance Eq CommentId

-- | Codecs
commentCodec :: JsonCodec Comment
commentCodec =
  CAR.object "Comment"
    { id: commentIdCodec
    , createdAt: dateTimeCodec
    , updatedAt: dateTimeCodec
    , body: CA.string
    , author: profileCodec
    }

commentIdCodec :: JsonCodec CommentId
commentIdCodec =
  wrapIso CommentId CA.int
