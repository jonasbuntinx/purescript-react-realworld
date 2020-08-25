module Conduit.Hook.Auth where

import Prelude
import Conduit.Data.Auth (Auth)
import Conduit.Data.Profile (UserProfile)
import Conduit.Env (Env)
import Data.Maybe (Maybe)
import React.Basic.Hooks as React
import Wire.React.Hooks (UseAtom, useAtomValue)

useAuth :: Env -> React.Hook (UseAtom (Maybe Auth)) (Maybe Auth)
useAuth { auth } = useAtomValue auth.signal

useProfile :: Env -> React.Hook (UseAtom (Maybe Auth)) (Maybe UserProfile)
useProfile { auth } = useAtomValue auth.signal <#> flip bind _.profile
