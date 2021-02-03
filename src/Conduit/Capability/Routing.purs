module Conduit.Capability.Routing where

import Prelude
import Conduit.Data.Route (Route)
import Data.Maybe (Maybe)
import FRP.Event (Event)
import React.Halo (HaloM, lift)

type RoutingInstance m
  = { readRouting :: m { route :: Route, prevRoute :: Maybe Route }
    , readRoutingEvent :: m (Event { route :: Route, prevRoute :: Maybe Route })
    , navigate :: Route -> m Unit
    , redirect :: Route -> m Unit
    }

class
  Monad m <= MonadRouting m where
  readRouting :: m { route :: Route, prevRoute :: Maybe Route }
  readRoutingEvent :: m (Event { route :: Route, prevRoute :: Maybe Route })
  navigate :: Route -> m Unit
  redirect :: Route -> m Unit

instance monadRoutingHaloM :: MonadRouting m => MonadRouting (HaloM props state action m) where
  readRouting = lift readRouting
  readRoutingEvent = lift readRoutingEvent
  navigate = lift <<< navigate
  redirect = lift <<< redirect
