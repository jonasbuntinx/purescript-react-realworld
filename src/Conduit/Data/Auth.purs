module Conduit.Data.Auth where

import Data.Moment (Moment)

type Auth
  = { token :: String
    , updated :: Moment
    }
