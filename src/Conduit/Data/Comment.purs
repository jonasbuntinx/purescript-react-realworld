module Conduit.Data.Comment where

import Prelude
import Conduit.Data.Profile (Profile)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Newtype (class Newtype, unwrap, wrap)
import Foreign.Day (DateTime)

type Comment
  = { id :: CommentId
    , createdAt :: DateTime
    , updatedAt :: DateTime
    , body :: String
    , author :: Profile
    }

newtype CommentId
  = CommentId Int

derive instance newtypeCommentId :: Newtype CommentId _

derive newtype instance eqCommentId :: Eq CommentId

instance decodeJsonCommentId :: DecodeJson CommentId where
  decodeJson = decodeJson >>> map wrap

instance encodeJsonCommentId :: EncodeJson CommentId where
  encodeJson = encodeJson <<< unwrap
