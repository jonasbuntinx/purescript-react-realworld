module Conduit.Env where

import Conduit.State.Routing (RoutingState)
import Conduit.State.User (UserState)

type Env
  = { userState :: UserState
    , routingState :: RoutingState
    }
