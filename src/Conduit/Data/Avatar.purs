module Conduit.Data.Avatar where

import Prelude

import Conduit.Assets as Assets
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Maybe (Maybe(..))
import Data.String as String

newtype Avatar
  = Avatar String

derive instance Eq Avatar

-- | Codecs
avatarCodec :: JsonCodec Avatar
avatarCodec = CA.prismaticCodec "Avatar" fromString toString CA.string

-- | Helpers
fromString :: String -> Maybe Avatar
fromString "" = Nothing
fromString str = Just (Avatar str)

toString :: Avatar -> String
toString (Avatar str) = str

withDefault :: Maybe Avatar -> Avatar
withDefault (Just av)
  | not $ String.null $ toString av = av
  | otherwise = default

withDefault Nothing = default

default :: Avatar
default = Avatar Assets.smileyCyrus

blank :: Avatar
blank = Avatar "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7"
