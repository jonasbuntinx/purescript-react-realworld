module Foreign.Moment where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (class Duration, Milliseconds, Seconds)
import Effect (Effect)
import Foreign.Generic (class Decode, class Encode, ForeignError(..), decode, encode)
import Simple.JSON (class ReadForeign, readImpl)

foreign import data Moment :: Type

foreign import _fromUTCString :: forall a. Fn3 a (Moment -> a) String a

fromUTCString :: String -> Maybe Moment
fromUTCString = runFn3 _fromUTCString Nothing Just

foreign import toUTCString :: Moment -> String

foreign import fromMilliseconds :: Milliseconds -> Moment

foreign import toMilliseconds :: Moment -> Milliseconds

foreign import now :: Effect Moment

foreign import format :: Format -> Moment -> String

foreign import unix :: Seconds -> Moment

newtype Format
  = Format String

instance eqMoment :: Eq Moment where
  eq a b = eq (toMilliseconds a) (toMilliseconds b)

instance ordMoment :: Ord Moment where
  compare a b = compare (toMilliseconds a) (toMilliseconds b)

instance semigroupMoment :: Semigroup Moment where
  append a b = fromMilliseconds (toMilliseconds a <> toMilliseconds b)

instance decodeMoment :: Decode Moment where
  decode =
    decode >=> fromUTCString
      >>> case _ of
          Nothing -> throwError $ pure $ ForeignError "Invalid datetime format (expecting UTC)"
          Just moment -> pure moment

instance encodeMoment :: Encode Moment where
  encode = encode <<< toUTCString

instance durationMoment :: Duration Moment where
  fromDuration = toMilliseconds
  toDuration = fromMilliseconds

instance readForeignMoment :: ReadForeign Moment where
  readImpl =
    readImpl >=> fromUTCString
      >>> case _ of
          Nothing -> throwError $ pure $ ForeignError "Invalid datetime format (expecting UTC)"
          Just moment -> pure moment
