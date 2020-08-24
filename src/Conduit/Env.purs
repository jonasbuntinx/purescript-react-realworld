module Conduit.Env where

import Conduit.Component.Auth (AuthEnv)
import Conduit.Component.Routing (RoutingEnv)

type Env
  = { auth :: AuthEnv
    , routing :: RoutingEnv
    }
