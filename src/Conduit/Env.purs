module Conduit.Env where

import Conduit.Env.Routing (RoutingSignal)
import Conduit.Env.Toast (ToastSignal)
import Conduit.Env.User (UserSignal)

type Env
  = { userSignal :: UserSignal
    , routingSignal :: RoutingSignal
    , toastSignal :: ToastSignal
    }
