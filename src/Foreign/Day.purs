module Foreign.Day (DateTime, fromUTCString, fromMilliseconds, toMilliseconds, now, format, Format(..), toDisplay) where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Foreign.Generic (ForeignError(..))
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

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

instance eqDateTime :: Eq DateTime where
  eq a b = eq (toMilliseconds a) (toMilliseconds b)

instance ordDateTime :: Ord DateTime where
  compare a b = compare (toMilliseconds a) (toMilliseconds b)

instance readForeignDateTime :: ReadForeign DateTime where
  readImpl =
    readImpl >=> fromUTCString
      >>> case _ of
          Nothing -> throwError $ pure $ ForeignError "Invalid datetime format (expecting UTC)"
          Just dateTime -> pure dateTime

instance writeForeignDateTime :: WriteForeign DateTime where
  writeImpl = writeImpl <<< toUTCString

toDisplay :: DateTime -> String
toDisplay = format (Format "MMMM Do, YYYY")
