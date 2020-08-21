module Conduit.Env where

import Prelude
import Conduit.Data.Route (Route)
import Conduit.Env.Auth (AuthSignal)
import Conduit.Env.Routing (RoutingSignal)
import Effect (Effect)

type Env
  = { authSignal :: AuthSignal
    , routingSignal :: RoutingSignal
    , navigate :: Route -> Effect Unit
    , redirect :: Route -> Effect Unit
    }
