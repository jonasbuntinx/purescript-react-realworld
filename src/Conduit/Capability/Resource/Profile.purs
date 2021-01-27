module Conduit.Capability.Resource.Profile where

import Prelude
import Conduit.Data.Error (Error)
import Conduit.Data.Profile (Profile)
import Conduit.Data.Username (Username)
import Data.Either (Either)
import React.Halo (HaloM, lift)

type ProfileImpl m
  = { getProfile :: Username -> m (Either Error Profile)
    , toggleFollow :: Profile -> m (Either Error Profile)
    }

class
  Monad m <= MonadProfile m where
  getProfile :: Username -> m (Either Error Profile)
  toggleFollow :: Profile -> m (Either Error Profile)

instance monadProfileHaloM :: MonadProfile m => MonadProfile (HaloM props state action m) where
  getProfile = lift <<< getProfile
  toggleFollow = lift <<< toggleFollow
