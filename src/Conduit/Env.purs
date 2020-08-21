module Conduit.Env where

import Prelude
import Conduit.Data.Auth (Auth)
import Conduit.Data.Profile (UserProfile)
import Conduit.Data.Route (Route)
import Conduit.Env.Auth (AuthSignal)
import Conduit.Env.Routing (RoutingSignal)
import Data.Maybe (Maybe)
import Effect (Effect)

type Env
  = { authSignal :: AuthSignal
    , readAuth :: Effect (Maybe Auth)
    , login :: String -> UserProfile -> Effect Unit
    , logout :: Effect Unit
    , updateProfile :: UserProfile -> Effect Unit
    , routingSignal :: RoutingSignal
    , navigate :: Route -> Effect Unit
    , redirect :: Route -> Effect Unit
    }
