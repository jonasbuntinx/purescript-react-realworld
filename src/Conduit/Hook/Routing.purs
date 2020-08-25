module Conduit.Hook.Routing where

import Conduit.Data.Route (Route)
import Conduit.Env (Env)
import React.Basic.Hooks as React
import Wire.React.Hooks (UseSignal, useSignal)

useRoute :: Env -> React.Hook (UseSignal Route) Route
useRoute { routing } = useSignal routing.signal
