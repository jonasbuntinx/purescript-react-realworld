module Conduit.Capability.Auth where

import Prelude
import Conduit.Data.Auth (Auth)
import Conduit.Data.Profile (UserProfile)
import Data.Maybe (Maybe)
import React.Halo (HaloM, lift)

class
  Monad m <= Auth m where
  read :: m (Maybe Auth)
  login :: String -> UserProfile -> m Unit
  logout :: m Unit
  updateProfile :: UserProfile -> m Unit

instance authHaloM :: Auth m => Auth (HaloM props state action m) where
  read = lift read
  login = \a b -> lift $ login a b
  logout = lift logout
  updateProfile = lift <<< updateProfile
