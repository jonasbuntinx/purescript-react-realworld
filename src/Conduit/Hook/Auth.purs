module Conduit.Hook.Auth where

import Prelude
import Conduit.Data.Profile (Profile)
import Conduit.Env.Auth (AuthSignal, Auth)
import Data.Maybe (Maybe)
import React.Basic.Hooks as React
import Wire.React (UseAtom, useAtomValue)

useAuth :: forall r. { authSignal :: AuthSignal | r } -> React.Hook (UseAtom (Maybe Auth)) (Maybe Auth)
useAuth { authSignal } = useAtomValue authSignal

useProfile :: forall r. { authSignal :: AuthSignal | r } -> React.Hook (UseAtom (Maybe Auth)) (Maybe Profile)
useProfile { authSignal } = useAtomValue authSignal <#> flip bind _.profile
