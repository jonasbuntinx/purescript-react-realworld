module Conduit.Data.Username where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Maybe (Maybe(..))

newtype Username
  = Username String

derive instance Eq Username

-- | Codecs
usernameCodec :: JsonCodec Username
usernameCodec = CA.prismaticCodec "Username" fromString toString CA.string

-- | Helpers
fromString :: String -> Maybe Username
fromString "" = Nothing

fromString str = Just (Username str)

toString :: Username -> String
toString (Username str) = str
