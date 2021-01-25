module Conduit.Capability.Routing where

import Prelude
import Conduit.Data.Route (Route)
import React.Halo (HaloM, lift)

class
  Monad m <= Routing m where
  navigate :: Route -> m Unit
  redirect :: Route -> m Unit
  logout :: m Unit

instance routingHaloM :: Routing m => Routing (HaloM props state action m) where
  navigate = lift <<< navigate
  redirect = lift <<< redirect
  logout = lift logout
