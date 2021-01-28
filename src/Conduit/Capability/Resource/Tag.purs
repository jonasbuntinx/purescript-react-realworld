module Conduit.Capability.Resource.Tag where

import Prelude
import Conduit.Data.Error (Error)
import Data.Either (Either)
import React.Halo (HaloM, lift)

-- | Tag
type TagInstance m
  = { listTags :: m (Either Error (Array String))
    }

class
  Monad m <= TagRepository m where
  listTags :: m (Either Error (Array String))

instance tagRepositoryHaloM :: TagRepository m => TagRepository (HaloM props state action m) where
  listTags = lift listTags
