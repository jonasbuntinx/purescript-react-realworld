module Conduit.Env where

import Conduit.Component.Auth (AuthEnv)
import Conduit.Component.Routing (RoutingEnv)
import Conduit.Data.Route (Route)

type Env
  = { auth :: AuthEnv
    , routing :: RoutingEnv Route
    }
