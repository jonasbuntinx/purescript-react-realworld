module Conduit.Capability.Resource.Profile where

import Prelude
import Conduit.Api.Client (Error)
import Conduit.Data.Profile (Profile)
import Conduit.Data.Username (Username)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either)
import React.Halo (HaloM)

class
  Monad m <=
  ProfileRepository m where
  getProfile :: Username -> m (Either Error Profile)
  toggleFollow :: Profile -> m (Either Error Profile)

instance ProfileRepository m => ProfileRepository (HaloM props ctx state action m) where
  getProfile = lift <<< getProfile
  toggleFollow = lift <<< toggleFollow
