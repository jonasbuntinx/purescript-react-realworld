module Conduit.Hook.User where

import Prelude
import Conduit.Data.Profile (Profile)
import Conduit.State.User (User, UserState, Auth)
import Data.Maybe (Maybe)
import Data.Tuple (fst, snd)
import React.Basic.Hooks as React
import Wire.React (UseAtom, useAtomValue)

useUser :: forall r. { userState :: UserState | r } -> React.Hook (UseAtom User) User
useUser { userState } = useAtomValue userState

useAuth :: forall r. { userState :: UserState | r } -> React.Hook (UseAtom User) (Maybe Auth)
useAuth { userState } = useAtomValue userState <#> fst

useProfile :: forall r. { userState :: UserState | r } -> React.Hook (UseAtom User) (Maybe Profile)
useProfile { userState } = useAtomValue userState <#> snd
