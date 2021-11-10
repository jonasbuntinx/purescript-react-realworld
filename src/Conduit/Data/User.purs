module Conduit.Data.User where

import Conduit.Data.Avatar (avatarCodec)
import Conduit.Data.Profile (ProfileRep)
import Conduit.Data.Username (usernameCodec)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR

type UserRep r
  = ( email :: String | ProfileRep r )

type User
  = { | UserRep () }

type CurrentUser
  = { | UserRep ( token :: String ) }

-- | Codecs
userCodec :: JsonCodec User
userCodec =
  CAR.object "User"
    { username: usernameCodec
    , bio: CAC.maybe CA.string
    , image: CAC.maybe avatarCodec
    , email: CA.string
    }

currentUserCodec :: JsonCodec CurrentUser
currentUserCodec =
  CAR.object "CurrentUser"
    { username: usernameCodec
    , bio: CAC.maybe CA.string
    , image: CAC.maybe avatarCodec
    , email: CA.string
    , token: CA.string
    }
