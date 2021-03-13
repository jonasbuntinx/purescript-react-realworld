module Conduit.Capability.Resource.Tag where

import Prelude
import Conduit.Api.Client (Error)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either)
import React.Halo (HaloM)

-- | Tag
type TagInstance m
  = { listTags :: m (Either Error (Array String))
    }

class
  Monad m <= TagRepository m where
  listTags :: m (Either Error (Array String))

instance tagRepositoryHaloM :: TagRepository m => TagRepository (HaloM props ctx state action m) where
  listTags = lift listTags
