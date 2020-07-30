module Conduit.Hook.User where

import Prelude
import Conduit.Data.Profile (Profile)
import Conduit.Env.User (User, UserSignal, Auth)
import Data.Maybe (Maybe)
import Data.Tuple (fst, snd)
import React.Basic.Hooks as React
import Wire.React (UseAtom, useAtomValue)

useUser :: forall r. { userSignal :: UserSignal | r } -> React.Hook (UseAtom User) User
useUser { userSignal } = useAtomValue userSignal

useAuth :: forall r. { userSignal :: UserSignal | r } -> React.Hook (UseAtom User) (Maybe Auth)
useAuth { userSignal } = useAtomValue userSignal <#> fst

useProfile :: forall r. { userSignal :: UserSignal | r } -> React.Hook (UseAtom User) (Maybe Profile)
useProfile { userSignal } = useAtomValue userSignal <#> snd
