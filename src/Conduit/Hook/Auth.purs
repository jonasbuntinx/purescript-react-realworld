module Conduit.Hook.Auth where

import Conduit.Data.Auth (Auth)
import Conduit.State.Auth (AuthState)
import Data.Maybe (Maybe)
import React.Basic.Hooks as React
import Wire.React (UseAtom, useAtomValue)

useAuth :: forall r. { authState :: AuthState | r } -> React.Hook (UseAtom (Maybe Auth)) (Maybe Auth)
useAuth { authState } = useAtomValue authState
