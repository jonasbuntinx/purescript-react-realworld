module Conduit.Data.Profile where

import Conduit.Data.Avatar (Avatar)
import Conduit.Data.Username (Username)
import Data.Maybe (Maybe)

type ProfileRep r
  = ( username :: Username
    , bio :: Maybe String
    , image :: Maybe Avatar
    | r
    )

type User
  = { | ProfileRep ( email :: String, token :: String ) }

type UserProfile
  = { | ProfileRep ( email :: String ) }

type Profile
  = { | ProfileRep ( following :: Boolean ) }
