module Conduit.Data.User where

import Conduit.Data.Profile (ProfileRep)

type User r
  = ( email :: String | ProfileRep r )

type CurrentUser
  = { | User ( token :: String ) }
