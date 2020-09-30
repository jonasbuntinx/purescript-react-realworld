module Conduit.Data.Comment where

import Prelude
import Apiary (class EncodeParam, encodeParam)
import Conduit.Data.Profile (Profile)
import Data.Newtype (class Newtype, unwrap, wrap)
import Foreign.Day (DateTime)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

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

instance readForeignCommentId :: ReadForeign CommentId where
  readImpl = readImpl >>> map wrap

instance writeForeignCommentId :: WriteForeign CommentId where
  writeImpl = writeImpl <<< unwrap

instance encodeParamCommentId :: EncodeParam CommentId where
  encodeParam = encodeParam <<< unwrap
