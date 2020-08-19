module Conduit.Env.Routing where

import Conduit.Data.Route (Route(..))
import Effect (Effect)
import Wire.React.Pure (Pure, create) as Pure

type RoutingSignal
  = Pure.Pure Route

create :: Effect RoutingSignal
create = Pure.create Home
