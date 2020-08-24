module Conduit.Hook.Auth where

import Prelude
import Conduit.Data.Profile (UserProfile)
import Conduit.Env (Env)
import Conduit.Env.Auth (AuthEnv)
import Data.Maybe (Maybe)
import React.Basic.Hooks as React
import Wire.React (UseAtom, useAtomValue)

useAuth :: Env -> React.Hook (UseAtom AuthEnv) AuthEnv
useAuth { auth } = useAtomValue auth

useProfile :: Env -> React.Hook (UseAtom AuthEnv) (Maybe UserProfile)
useProfile { auth } = useAtomValue auth <#> flip bind _.profile
