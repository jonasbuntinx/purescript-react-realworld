module Conduit.Hook.Routing where

import Prelude
import Conduit.Data.Route (Route)
import Conduit.Env (Env)
import Conduit.Env.Routing (RoutingEnv)
import React.Basic.Hooks as React
import Wire.React (UseAtom, useAtomValue)

useRoute :: Env -> React.Hook (UseAtom RoutingEnv) Route
useRoute { routing } = useAtomValue routing <#> _.route
