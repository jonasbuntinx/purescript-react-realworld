module Conduit.Data.Comment where

import Prelude
import Apiary.Url as Url
import Conduit.Data.Profile (Author)
import Data.Newtype (class Newtype, unwrap, wrap)
import Foreign.Moment (Moment)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

type Comment
  = { id :: CommentId
    , createdAt :: Moment
    , updatedAt :: Moment
    , body :: String
    , author :: Author
    }

newtype CommentId
  = CommentId Int

derive instance newtypeCommentId :: Newtype CommentId _

derive newtype instance eqCommentId :: Eq CommentId

derive newtype instance ordCommentId :: Ord CommentId

instance readForeignCommentId :: ReadForeign CommentId where
  readImpl = readImpl >>> map wrap

instance writeForeignCommentId :: WriteForeign CommentId where
  writeImpl = writeImpl <<< unwrap

instance encodeParamCommentId :: Url.EncodeParam CommentId where
  encodeParam = Url.encodeParam <<< unwrap
