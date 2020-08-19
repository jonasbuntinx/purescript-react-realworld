module Conduit.Data.Avatar
  ( Avatar
  , fromString
  , toString
  , withDefault
  , default
  , blank
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.String as String
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype Avatar
  = Avatar String

derive instance eqAvatar :: Eq Avatar

derive instance ordAvatar :: Ord Avatar

derive newtype instance writeForeignAvatar :: WriteForeign Avatar

derive newtype instance readForeignAvatar :: ReadForeign Avatar

fromString :: String -> Avatar
fromString str = Avatar str

toString :: Avatar -> String
toString (Avatar str) = str

withDefault :: Maybe Avatar -> Avatar
withDefault (Just av)
  | not $ String.null $ toString av = av
  | otherwise = default

withDefault Nothing = default

default :: Avatar
default = fromString "https://static.productionready.io/images/smiley-cyrus.jpg"

blank :: Avatar
blank = fromString "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7"
