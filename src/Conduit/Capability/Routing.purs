module Conduit.Capability.Routing where

import Prelude
import React.Halo (HaloM, lift)

class
  Monad m <= Routing route m where
  navigate :: route -> m Unit
  redirect :: route -> m Unit

instance routingHaloM :: Routing route m => Routing route (HaloM props state action m) where
  navigate = lift <<< navigate
  redirect = lift <<< redirect
