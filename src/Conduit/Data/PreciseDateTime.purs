module Conduit.Data.PreciseDateTime where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.DateTime (DateTime)
import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.List (List, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.PreciseDateTime (PreciseDateTime, toDateTimeLossy, fromRFC3339String) as PDT
import Data.RFC3339String (RFC3339String(..))
import Foreign (ForeignError(..))
import Simple.JSON (class ReadForeign, readImpl)

newtype PreciseDateTime
  = PreciseDateTime PDT.PreciseDateTime

derive instance newtypePreciseDateTime :: Newtype PreciseDateTime _

instance readForeignPreciseDateTime :: ReadForeign PreciseDateTime where
  readImpl =
    readImpl
      >=> fromString
      >>> case _ of
          Just preciseDateTime -> pure preciseDateTime
          Nothing -> throwError $ pure $ ForeignError "Failed to decode preciseDateTime"

fromString :: String -> Maybe PreciseDateTime
fromString =
  map PreciseDateTime
    <<< PDT.fromRFC3339String
    <<< RFC3339String

toDateTime :: PreciseDateTime -> DateTime
toDateTime = unwrap >>> PDT.toDateTimeLossy

toDisplay :: PreciseDateTime -> String
toDisplay = toDateTime >>> format dateFormatter
  where
  dateFormatter :: List FormatterCommand
  dateFormatter =
    fromFoldable
      [ DayOfWeekNameShort
      , Placeholder " "
      , MonthShort
      , Placeholder " "
      , DayOfMonth
      , Placeholder ", "
      , YearFull
      ]
