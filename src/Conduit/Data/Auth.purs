module Conduit.Data.Auth where

import Prelude
import Conduit.Data.Jwt as Jwt
import Conduit.Data.User (User)
import Conduit.Data.Username (Username)
import Data.Either (hush)
import Data.Maybe (Maybe)
import Data.Time.Duration (Milliseconds(..))
import Foreign.Day (DateTime, fromMilliseconds)

type Auth
  =
  { token :: String
  , username :: Username
  , expirationTime :: DateTime
  , user :: Maybe User
  }

-- | Helpers
toAuth :: String -> Maybe User -> Maybe Auth
toAuth token user = do
  { exp, username } <- hush $ Jwt.decode token
  pure { token, username, expirationTime: fromMilliseconds $ Milliseconds $ exp * 1000.0, user }
