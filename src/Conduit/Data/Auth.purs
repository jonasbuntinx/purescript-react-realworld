module Conduit.Data.Auth where

import Prelude
import Conduit.Data.Jwt as Jwt
import Conduit.Data.Profile (UserProfile)
import Conduit.Data.Username (Username)
import Data.Either (hush)
import Data.Maybe (Maybe)
import Data.Time.Duration (Milliseconds(..))
import Foreign.Day (DateTime, fromMilliseconds)

type Auth
  = { token :: String
    , username :: Username
    , expirationTime :: DateTime
    , profile :: Maybe UserProfile
    }

-- | Helpers
toAuth :: String -> Maybe UserProfile -> Maybe Auth
toAuth token profile = do
  { exp, username } <- hush $ Jwt.decode token
  pure { token, username, expirationTime: fromMilliseconds $ Milliseconds $ exp * 1000.0, profile }
