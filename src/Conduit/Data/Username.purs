module Conduit.Data.Username where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core as AC
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Maybe (Maybe(..))

newtype Username
  = Username String

derive instance eqUsername :: Eq Username

derive newtype instance encodeJsonUsername :: EncodeJson Username

instance decodeJsonUsername :: DecodeJson Username where
  decodeJson =
    decodeJson >=> fromString
      >>> case _ of
          Just username -> pure username
          Nothing -> throwError $ UnexpectedValue $ AC.fromString "Failed to decode username"

fromString :: String -> Maybe Username
fromString "" = Nothing

fromString str = Just (Username str)

toString :: Username -> String
toString (Username str) = str
