module Conduit.Env where

import Conduit.Env.Auth (AuthSignal)
import Conduit.Env.Routing (RoutingSignal)

type Env
  = { authSignal :: AuthSignal
    , routingSignal :: RoutingSignal
    }
