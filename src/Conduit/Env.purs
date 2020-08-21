module Conduit.Env where

import Conduit.Env.Auth (AuthEnv)
import Conduit.Env.Routing (RoutingEnv)

type Env
  = { auth :: AuthEnv, routing :: RoutingEnv }
