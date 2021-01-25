module Conduit.Hook.Auth where

import Prelude
import Conduit.Data.Auth (Auth)
import Conduit.Data.Env (Env)
import Conduit.Data.User (User)
import Data.Maybe (Maybe)
import React.Basic.Hooks as React
import Wire.React.Hooks (UseAtom, useAtomValue)

useAuth :: Env -> React.Hook (UseAtom (Maybe Auth)) (Maybe Auth)
useAuth { auth } = useAtomValue auth.signal

useUser :: Env -> React.Hook (UseAtom (Maybe Auth)) (Maybe { | User () })
useUser { auth } = useAtomValue auth.signal <#> flip bind _.user
