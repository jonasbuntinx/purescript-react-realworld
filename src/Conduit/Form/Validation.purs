module Conduit.Form.Validation where

import Prelude
import Conduit.Data.Username (Username)
import Conduit.Data.Username as Username
import Data.Either (fromRight)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Flags
import Data.Validation.Semigroup (V(..))
import Data.Validation.Semigroup as V
import Partial.Unsafe (unsafePartial)

-- | String validators
validateNonEmpty :: String -> V (Array String) String
validateNonEmpty input
  | String.null input = V.invalid $ pure "is required"
  | otherwise = V $ pure input

validateEmailFormat :: String -> V (Array String) String
validateEmailFormat input
  | not $ isValidEmail input = V.invalid $ pure "is invalid"
  | otherwise = V $ pure input

isValidEmail :: String -> Boolean
isValidEmail = Regex.test emailRegex
  where
  emailRegex = unsafePartial $ fromRight $ Regex.regex """^[a-zA-Z0-9.!#$%&'*+\/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*$""" Flags.global

validateMinimumLength :: Int -> String -> V (Array String) String
validateMinimumLength validLength input
  | String.length input <= validLength = V.invalid $ pure "is too short"
  | otherwise = V $ pure input

validateMaximunLength :: Int -> String -> V (Array String) String
validateMaximunLength validLength input
  | String.length input > validLength = V.invalid $ pure "is too long"
  | otherwise = V $ pure input

-- | Username validator
validateUsernameFormat :: String -> V (Array String) Username
validateUsernameFormat input = case Username.parse input of
  Nothing -> V.invalid $ pure "is invalid"
  Just username -> V $ pure username
