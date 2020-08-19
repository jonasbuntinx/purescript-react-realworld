module Conduit.Data.Username where

import Prelude
import Apiary.Url as Url
import Control.Monad.Error.Class (throwError)
import Data.Maybe (Maybe(..))
import Foreign (ForeignError(..))
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl)

newtype Username
  = Username String

derive instance eqUsername :: Eq Username

derive newtype instance writeForeignUsername :: WriteForeign Username

instance readForeignUsername :: ReadForeign Username where
  readImpl =
    readImpl
      >=> fromString
      >>> case _ of
          Just username -> pure username
          Nothing -> throwError $ pure $ ForeignError "Failed to decode username"

derive newtype instance encodeParamUsername :: Url.EncodeParam Username

fromString :: String -> Maybe Username
fromString "" = Nothing

fromString str = Just (Username str)

toString :: Username -> String
toString (Username str) = str
