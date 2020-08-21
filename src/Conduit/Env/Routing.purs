module Conduit.Env.Routing where

import Prelude
import Conduit.Data.Route (Route(..))
import Effect (Effect)
import Wire.React.Pure (Pure, create) as Pure

type RoutingEnv
  = { signal :: RoutingSignal
    , navigate :: Route -> Effect Unit
    , redirect :: Route -> Effect Unit
    }

type RoutingSignal
  = Pure.Pure Route

create :: Effect RoutingSignal
create = Pure.create Home
