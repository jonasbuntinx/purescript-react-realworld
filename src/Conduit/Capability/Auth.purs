module Conduit.Capability.Auth where

import Prelude
import Conduit.Data.Auth (Auth)
import Conduit.Data.Profile (UserProfile)
import Data.Maybe (Maybe)

class MonadAuth m where
  read :: m (Maybe Auth)
  login :: String -> UserProfile -> m Unit
  logout :: m Unit
  updateProfile :: UserProfile -> m Unit
