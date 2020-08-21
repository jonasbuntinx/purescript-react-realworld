module Conduit.Hook.Routing where

import Conduit.Data.Route (Route)
import Conduit.Env (Env)
import React.Basic.Hooks as React
import Wire.React (UseAtom, useAtomValue)

useRoute :: Env -> React.Hook (UseAtom Route) Route
useRoute { routing } = useAtomValue routing.signal
