module Conduit.Hook.Routing where

import Prelude
import Conduit.Data.Route (Route)
import Conduit.Data.Transition (Transition, _Transition)
import Conduit.State.Routing (RoutingState)
import Data.Lens (view)
import React.Basic.Hooks as React
import Wire.React (UseAtom, useAtomValue)

useTransition :: forall r. { routingState :: RoutingState | r } -> React.Hook (UseAtom Transition) Transition
useTransition { routingState } = useAtomValue routingState

useRoute :: forall r. { routingState :: RoutingState | r } -> React.Hook (UseAtom Transition) Route
useRoute { routingState } = view _Transition <$> useAtomValue routingState
