module Conduit.Env where

import Conduit.State.Auth (AuthState)
import Conduit.State.Routing (RoutingState)

type Env
  = { authState :: AuthState
    , routingState :: RoutingState
    }
