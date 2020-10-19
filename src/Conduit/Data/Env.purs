module Conduit.Data.Env where

import Prelude
import Conduit.Data.Auth (Auth)
import Conduit.Data.Route (Route)
import Data.Maybe (Maybe)
import Effect (Effect)
import Wire.React.Atom.Sync as Sync
import Wire.Signal as Signal

type Env
  = { auth ::
        { signal :: Sync.Sync (Maybe Auth)
        }
    , router ::
        { signal :: Signal.Signal Route
        , navigate :: Route -> Effect Unit
        , redirect :: Route -> Effect Unit
        }
    }
