module Foreign.Day (DateTime, fromUTCString, toUTCString, fromMilliseconds, toMilliseconds, now, format, Format(..), dateTimeCodec, toDisplay) where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds)
import Effect (Effect)

foreign import data DateTime :: Type

foreign import _fromUTCString :: forall a. Fn3 a (DateTime -> a) String a

fromUTCString :: String -> Maybe DateTime
fromUTCString = runFn3 _fromUTCString Nothing Just

foreign import toUTCString :: DateTime -> String

foreign import fromMilliseconds :: Milliseconds -> DateTime

foreign import toMilliseconds :: DateTime -> Milliseconds

foreign import now :: Effect DateTime

foreign import format :: Format -> DateTime -> String

newtype Format
  = Format String

instance Eq DateTime where
  eq a b = eq (toMilliseconds a) (toMilliseconds b)

instance Ord DateTime where
  compare a b = compare (toMilliseconds a) (toMilliseconds b)

-- | Codecs
dateTimeCodec :: JsonCodec DateTime
dateTimeCodec = CA.prismaticCodec "DateTime" fromUTCString toUTCString CA.string

-- | Helpers
toDisplay :: DateTime -> String
toDisplay = format (Format "MMMM Do, YYYY")
