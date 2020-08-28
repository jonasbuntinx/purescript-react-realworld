module Conduit.Data.Slug where

import Prelude
import Apiary.Url as Url
import Control.Monad.Error.Class (throwError)
import Data.Either (fromRight)
import Data.Maybe (Maybe(..))
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Flags
import Foreign (ForeignError(..))
import Partial.Unsafe (unsafePartial)
import Simple.JSON (class ReadForeign, readImpl)

newtype Slug
  = Slug String

derive instance eqSlug :: Eq Slug

instance readForeignSlug :: ReadForeign Slug where
  readImpl =
    readImpl >=> fromString
      >>> case _ of
          Just slug -> pure slug
          Nothing -> throwError $ pure $ ForeignError "Failed to decode slug"

derive newtype instance encodeParamSlug :: Url.EncodeParam Slug

fromString :: String -> Maybe Slug
fromString str = if Regex.test slugRegex str then Just $ Slug str else Nothing
  where
  slugRegex = unsafePartial $ fromRight $ Regex.regex """^[_a-z0-9]*(?:-[_a-z0-9]+)*$""" Flags.global

toString :: Slug -> String
toString (Slug s) = s
