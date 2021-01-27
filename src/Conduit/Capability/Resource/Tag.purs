module Conduit.Capability.Resource.Tag where

import Prelude
import Conduit.Data.Error (Error)
import Data.Either (Either)
import React.Halo (HaloM, lift)

-- | Tag
type TagInst m
  = { listTags :: m (Either Error (Array String))
    }

class
  Monad m <= MonadTag m where
  listTags :: m (Either Error (Array String))

instance monadTagHaloM :: MonadTag m => MonadTag (HaloM props state action m) where
  listTags = lift listTags
