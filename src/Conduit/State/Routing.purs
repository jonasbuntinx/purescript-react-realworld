module Conduit.State.Routing where

import Prelude
import Conduit.Data.Route (Route(..))
import Conduit.Data.Transition (Transition(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Wire.React.Pure (Pure, create) as Pure

type RoutingState
  = Pure.Pure Transition

create :: Effect RoutingState
create = Pure.create $ Loading Nothing Home
