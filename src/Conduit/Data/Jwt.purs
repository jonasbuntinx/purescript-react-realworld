module Conduit.Data.Jwt where

import Prelude
import Conduit.Data.Username (Username, usernameCodec)
import Data.Codec as Codec
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either, note)
import Data.Lens (_Left, over)
import Data.String as String
import Effect.Exception as Exception
import Foreign.Base64 (atob)

type Jwt
  =
  { username :: Username
  , exp :: Number
  }

data Error
  = MalformedToken
  | Base64DecodeError Exception.Error
  | JSONParseError String
  | JSONDecodeError CA.JsonDecodeError

-- | Helpers
decode :: String -> Either Error Jwt
decode =
  let
    payload = note MalformedToken <<< (_ `Array.index` 1) <<< String.split (String.Pattern ".")

    decodeBase64 = map (over _Left Base64DecodeError) atob

    parseJSON = map (over _Left JSONParseError) jsonParser

    decodeJSON =
      map (over _Left JSONDecodeError)
        ( Codec.decode
            $ CAR.object "Jwt"
                { username: usernameCodec
                , exp: CA.number
                }
        )
  in
    payload >=> decodeBase64 >=> parseJSON >=> decodeJSON
