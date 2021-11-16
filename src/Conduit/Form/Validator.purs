module Conduit.Form.Validator where

import Prelude
import Conduit.Data.Username (Username)
import Conduit.Data.Username as Username
import Data.Either (Either(..), note)
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Flags
import Data.Validation.Semigroup (V(..), invalid)
import Partial.Unsafe (unsafePartial)

type Validator result valid =
  result -> V (Array String) valid

nonEmpty :: Validator String String
nonEmpty = mustBe (not <<< String.null) "is required"

validEmail :: Validator String String
validEmail = mustBe (Regex.test emailRegex) "is invalid"
  where
  emailRegex = unsafePartial $ (\(Right a) -> a) $ Regex.regex """^[a-zA-Z0-9.!#$%&'*+\/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*$""" Flags.global

minimumLength :: Int -> Validator String String
minimumLength validLength = mustBe (\input -> String.length input >= validLength) "is too short"

maximunLength :: Int -> Validator String String
maximunLength validLength = mustBe (\input -> String.length input <= validLength) "is too long"

validUsername :: Validator String Username
validUsername = V <<< (note (pure "is invalid") <<< Username.fromString)

mustBe :: forall a. (a -> Boolean) -> String -> Validator a a
mustBe cond error value
  | cond value = pure value
  | otherwise = invalid $ pure error
