module Conduit.Data.Auth where

import Prelude
import Conduit.Data.Jwt as Jwt
import Conduit.Data.User (User)
import Conduit.Data.Username (Username)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Time.Duration (Milliseconds(..))
import Foreign.Day (DateTime, fromMilliseconds)

type Auth =
  { token :: String
  , username :: Username
  , expirationTime :: DateTime
  , user :: Maybe User
  }

-- | Helpers
toAuth :: String -> Maybe User -> Either String Auth
toAuth token user = do
  let jwt = Jwt.decode token
  case jwt of
    Left e -> Left $ show e
    Right { exp, username } ->
      pure { token, username, expirationTime: fromMilliseconds $ Milliseconds $ exp * 1000.0, user }
