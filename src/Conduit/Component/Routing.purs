module Conduit.Component.Routing where

import Prelude
import Effect (Effect)
import Wire.React.Router (Route)
import Wire.Signal as Signal

type RoutingEnv route
  = { signal :: Signal.Signal (Route route)
    , navigate :: route -> Effect Unit
    , redirect :: route -> Effect Unit
    }
