module Conduit.Capability.Routing where

import Prelude
import Conduit.Data.Route (Route)
import FRP.Event (Event)
import React.Halo (HaloM, lift)

type RoutingInst m
  = { readRoute :: m Route
    , readRoutingEvent :: m (Event Route)
    , navigate :: Route -> m Unit
    , redirect :: Route -> m Unit
    }

class
  Monad m <= MonadRouting m where
  readRoute :: m Route
  readRoutingEvent :: m (Event Route)
  navigate :: Route -> m Unit
  redirect :: Route -> m Unit

instance monadRoutingHaloM :: MonadRouting m => MonadRouting (HaloM props state action m) where
  readRoute = lift readRoute
  readRoutingEvent = lift readRoutingEvent
  navigate = lift <<< navigate
  redirect = lift <<< redirect
