module Conduit.Hook.Router where

import Prelude
import Conduit.Data.Route (Route)
import Conduit.Env (Env)
import Data.Lens (view)
import React.Basic.Hooks as React
import Wire.React.Hooks (UseSignal, useSignal)
import Wire.React.Router (_Route)
import Wire.React.Router as Router

useRoute :: Env -> React.Hook (UseSignal (Router.Route Route)) Route
useRoute { router } = view _Route <$> useSignal router.signal
