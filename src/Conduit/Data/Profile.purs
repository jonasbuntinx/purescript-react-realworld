module Conduit.Data.Profile where

import Conduit.Data.Username (Username)
import Data.Maybe (Maybe)

type ProfileRep r
  = ( username :: Username
    , bio :: Maybe String
    , image :: Maybe String
    | r
    )

type User
  = { | ProfileRep ( email :: String, token :: String ) }

type Profile
  = { | ProfileRep ( email :: String ) }

type Author
  = { | ProfileRep ( following :: Boolean ) }
