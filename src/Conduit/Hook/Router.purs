module Conduit.Hook.Router where

import Conduit.Data.Route (Route)
import Conduit.Data.Env (Env)
import React.Basic.Hooks as React
import Wire.React.Hooks (UseSignal, useSignal)

useRoute :: Env -> React.Hook (UseSignal Route) Route
useRoute { router } = useSignal router.signal
