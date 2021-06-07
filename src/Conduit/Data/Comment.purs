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

derive instance Newtype CommentId _

derive newtype instance Eq CommentId

instance DecodeJson CommentId where
  decodeJson = decodeJson >>> map wrap

instance EncodeJson CommentId where
  encodeJson = encodeJson <<< unwrap
