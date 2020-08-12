module Conduit.Data.Jwt where

import Prelude
import Conduit.Data.Username (Username)
import Data.Array as Array
import Data.Either (Either, note)
import Data.Lens (_Left, over)
import Data.String as String
import Effect.Exception as Exception
import Foreign (MultipleErrors)
import Foreign.Base64 (atob)
import Simple.JSON (readJSON)

type Jwt
  = { username :: Username, exp :: Number }

data Error
  = MalformedToken
  | Base64DecodeError Exception.Error
  | JSONDecodeError MultipleErrors

decode :: String -> Either Error Jwt
decode =
  let
    payload = note MalformedToken <<< (_ `Array.index` 1) <<< String.split (String.Pattern ".")

    decodeBase64 = map (over _Left Base64DecodeError) atob

    decodeJSON = map (over _Left JSONDecodeError) readJSON
  in
    payload >=> decodeBase64 >=> decodeJSON
