module Conduit.Capability.Routing where

import Prelude
import Conduit.Data.Route (Route)
import Control.Monad.Trans.Class (lift)
import Halogen.Subscription (Emitter)
import React.Halo (HaloM)

type RoutingInstance m
  = { readRoute :: m Route
    , readRoutingEvent :: m (Emitter Route)
    , navigate :: Route -> m Unit
    , redirect :: Route -> m Unit
    }

class
  Monad m <= MonadRouting m where
  readRoute :: m Route
  readRoutingEvent :: m (Emitter Route)
  navigate :: Route -> m Unit
  redirect :: Route -> m Unit

instance monadRoutingHaloM :: MonadRouting m => MonadRouting (HaloM props ctx state action m) where
  readRoute = lift readRoute
  readRoutingEvent = lift readRoutingEvent
  navigate = lift <<< navigate
  redirect = lift <<< redirect
