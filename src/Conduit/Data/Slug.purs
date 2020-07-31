module Conduit.Data.Slug
  ( Slug
  , generate
  , parse
  , toString
  , truncate
  ) where

import Prelude
import Apiary.Url as Url
import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Char.Unicode (isAlphaNum, isLatin1)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Foreign (ForeignError(..))
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl)

newtype Slug
  = Slug String

derive instance genericSlug :: Generic Slug _

derive instance eqSlug :: Eq Slug

derive instance ordSlug :: Ord Slug

derive newtype instance semigroupSlug :: Semigroup Slug

instance showSlug :: Show Slug where
  show = genericShow

derive newtype instance writeForeignSlug :: WriteForeign Slug

instance readForgeignSlug :: ReadForeign Slug where
  readImpl =
    readImpl
      >=> parse
      >>> case _ of
          Just username -> pure username
          Nothing -> throwError $ pure $ ForeignError "Failed to decode slug"

derive newtype instance encodeParamSlug :: Url.EncodeParam Slug

generate :: String -> Maybe Slug
generate s = do
  let
    arr = words $ String.toLower $ onlyAlphaNum $ stripApostrophes s
  if Array.null arr then
    Nothing
  else
    Just $ Slug $ String.joinWith "-" arr
  where
  stripApostrophes = String.replaceAll (Pattern "'") (Replacement "")

  onlyAlphaNum =
    fromCharArray
      <<< map (\x -> if isAlphaNum x && isLatin1 x then x else ' ')
      <<< toCharArray

  words = Array.filter (not String.null) <<< String.split (Pattern " ")

parse :: String -> Maybe Slug
parse str = generate str >>= check
  where
  check slug@(Slug s)
    | s == str = Just slug
    | otherwise = Nothing

toString :: Slug -> String
toString (Slug s) = s

truncate :: Int -> Slug -> Maybe Slug
truncate n (Slug s)
  | n < 1 = Nothing
  | n >= String.length s = Just (Slug s)
  | otherwise = generate $ String.take n s
