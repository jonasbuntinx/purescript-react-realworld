module Conduit.Data.Slug where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core as AC
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Either (fromRight)
import Data.Maybe (Maybe(..))
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Flags
import Partial.Unsafe (unsafePartial)

newtype Slug
  = Slug String

derive instance eqSlug :: Eq Slug

instance decodeJsonSlug :: DecodeJson Slug where
  decodeJson =
    decodeJson >=> fromString
      >>> case _ of
          Just slug -> pure slug
          Nothing -> throwError $ UnexpectedValue $ AC.fromString "Failed to decode slug"

fromString :: String -> Maybe Slug
fromString str = if Regex.test slugRegex str then Just $ Slug str else Nothing
  where
  slugRegex = unsafePartial $ fromRight $ Regex.regex """^[_a-z0-9]*(?:-[_a-z0-9]+)*$""" Flags.global

toString :: Slug -> String
toString (Slug s) = s
