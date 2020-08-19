module Conduit.Hook.Routing where

import Conduit.Data.Route (Route)
import Conduit.Env.Routing (RoutingSignal)
import React.Basic.Hooks as React
import Wire.React (UseAtom, useAtomValue)

useRoute :: forall r. { routingSignal :: RoutingSignal | r } -> React.Hook (UseAtom Route) Route
useRoute { routingSignal } = useAtomValue routingSignal
