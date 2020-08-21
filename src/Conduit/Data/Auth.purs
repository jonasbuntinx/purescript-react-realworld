module Conduit.Data.Auth where

import Conduit.Data.Profile (UserProfile)
import Conduit.Data.Username (Username)
import Data.Maybe (Maybe)
import Foreign.Day (DateTime)

type Auth
  = { token :: String
    , username :: Username
    , expirationTime :: DateTime
    , profile :: Maybe UserProfile
    }
