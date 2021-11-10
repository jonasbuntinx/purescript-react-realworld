module Conduit.Data.Slug where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Flags
import Partial.Unsafe (unsafePartial)

newtype Slug
  = Slug String

derive instance Eq Slug

-- | Codecs
slugCodec :: JsonCodec Slug
slugCodec =
  CA.prismaticCodec "Slug" fromString toString CA.string

-- | Helpers
fromString :: String -> Maybe Slug
fromString str = if Regex.test slugRegex str then Just $ Slug str else Nothing
  where
  slugRegex = unsafePartial $ (\(Right a) -> a) $ Regex.regex """^[_a-zA-Z0-9]*(?:-[_a-zA-Z0-9]+)*$""" Flags.global

toString :: Slug -> String
toString (Slug s) = s
