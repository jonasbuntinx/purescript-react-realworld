module Conduit.Hook.App where

import Prelude
import Conduit.AppM (AppM, runAppM)
import Conduit.Env (Env)
import Effect (Effect)
import React.Basic.Hooks (Hook, UseEffect, useEffect)

useAppEffect :: forall deps. Eq deps => Env -> deps -> AppM Effect Unit -> Hook (UseEffect deps) Unit
useAppEffect env deps effect = useEffect deps (runAppM env effect *> mempty)
