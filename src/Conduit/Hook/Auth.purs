module Conduit.Hook.Auth where

import Prelude
import Conduit.Data.Profile (UserProfile)
import Conduit.Env.Auth (AuthSignal, Auth)
import Data.Maybe (Maybe)
import React.Basic.Hooks as React
import Wire.React (UseAtom, useAtomValue)

useAuth :: forall r. { authSignal :: AuthSignal | r } -> React.Hook (UseAtom (Maybe Auth)) (Maybe Auth)
useAuth { authSignal } = useAtomValue authSignal

useProfile :: forall r. { authSignal :: AuthSignal | r } -> React.Hook (UseAtom (Maybe Auth)) (Maybe UserProfile)
useProfile { authSignal } = useAtomValue authSignal <#> flip bind _.profile
