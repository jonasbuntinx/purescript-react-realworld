module Conduit.Hook.Routing where

import Prelude
import Conduit.Data.Route (Route)
import Conduit.Data.Transition (Transition, _Transition)
import Conduit.Env.Routing (RoutingSignal)
import Data.Lens (view)
import React.Basic.Hooks as React
import Wire.React (UseAtom, useAtomValue)

useTransition :: forall r. { routingSignal :: RoutingSignal | r } -> React.Hook (UseAtom Transition) Transition
useTransition { routingSignal } = useAtomValue routingSignal

useRoute :: forall r. { routingSignal :: RoutingSignal | r } -> React.Hook (UseAtom Transition) Route
useRoute { routingSignal } = view _Transition <$> useAtomValue routingSignal
