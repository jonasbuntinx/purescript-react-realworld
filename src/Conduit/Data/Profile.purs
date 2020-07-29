module Conduit.Data.Profile where

import Data.Maybe (Maybe)

type ProfileRep r
  = ( username :: String
    , bio :: Maybe String
    , image :: Maybe String
    | r
    )

type Profile
  = { | ProfileRep () }
