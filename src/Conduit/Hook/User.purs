module Conduit.Hook.User where

import Prelude
import Conduit.Data.Profile (Profile)
import Conduit.Env.User (UserSignal, Auth)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple, fst, snd)
import React.Basic.Hooks as React
import Wire.React (UseAtom, useAtomValue)

useUser :: forall r. { userSignal :: UserSignal | r } -> React.Hook (UseAtom (Tuple (Maybe Auth) (Maybe Profile))) (Tuple (Maybe Auth) (Maybe Profile))
useUser { userSignal } = useAtomValue userSignal

useAuth :: forall r. { userSignal :: UserSignal | r } -> React.Hook (UseAtom (Tuple (Maybe Auth) (Maybe Profile))) (Maybe Auth)
useAuth { userSignal } = useAtomValue userSignal <#> fst

useProfile :: forall r. { userSignal :: UserSignal | r } -> React.Hook (UseAtom (Tuple (Maybe Auth) (Maybe Profile))) (Maybe Profile)
useProfile { userSignal } = useAtomValue userSignal <#> snd
