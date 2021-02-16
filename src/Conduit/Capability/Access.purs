module Conduit.Capability.Access where

import Prelude
import Conduit.Data.Access (Access)
import Conduit.Data.Auth (Auth)
import FRP.Event (Event)
import React.Halo (HaloM, lift)

type AccessInstance m
  = { readAccess :: m (Access Auth)
    , readAccessEvent :: m (Event (Access Auth))
    , modifyAccess :: (Access Auth -> Access Auth) -> m (Access Auth)
    }

class
  Monad m <= MonadAccess m where
  readAccess :: m (Access Auth)
  readAccessEvent :: m (Event (Access Auth))
  modifyAccess :: (Access Auth -> Access Auth) -> m (Access Auth)

instance monadAccessHaloM :: MonadAccess m => MonadAccess (HaloM props state action m) where
  readAccess = lift readAccess
  readAccessEvent = lift readAccessEvent
  modifyAccess = lift <<< modifyAccess
