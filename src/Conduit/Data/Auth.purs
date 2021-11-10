module Conduit.Data.Auth where

import Prelude
import Conduit.Data.Jwt as Jwt
import Conduit.Data.User (User, userCodec)
import Conduit.Data.Username (Username, usernameCodec)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Either (hush)
import Data.Maybe (Maybe)
import Data.Time.Duration (Milliseconds(..))
import Foreign.Day (DateTime, dateTimeCodec, fromMilliseconds)

type Auth
  = { token :: String
    , username :: Username
    , expirationTime :: DateTime
    , user :: Maybe User
    }

-- | Codecs
authCodec :: JsonCodec Auth
authCodec =
  CAR.object "Auth"
    { token: CA.string
    , username: usernameCodec
    , expirationTime: dateTimeCodec
    , user: CAC.maybe userCodec
    }

-- | Helpers
toAuth :: String -> Maybe User -> Maybe Auth
toAuth token user = do
  { exp, username } <- hush $ Jwt.decode token
  pure { token, username, expirationTime: fromMilliseconds $ Milliseconds $ exp * 1000.0, user }
