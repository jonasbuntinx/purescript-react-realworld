module Conduit.Capability.Resource.Profile where

import Prelude
import Conduit.Api.Client (Error)
import Conduit.Data.Profile (Profile)
import Conduit.Data.Username (Username)
import Data.Either (Either)
import React.Halo (HaloM, lift)

type ProfileInstance m
  = { getProfile :: Username -> m (Either Error Profile)
    , toggleFollow :: Profile -> m (Either Error Profile)
    }

class
  Monad m <= ProfileRepository m where
  getProfile :: Username -> m (Either Error Profile)
  toggleFollow :: Profile -> m (Either Error Profile)

instance profileRepositoryHaloM :: ProfileRepository m => ProfileRepository (HaloM props state action m) where
  getProfile = lift <<< getProfile
  toggleFollow = lift <<< toggleFollow
