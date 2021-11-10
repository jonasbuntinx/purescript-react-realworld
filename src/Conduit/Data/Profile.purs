module Conduit.Data.Profile where

import Conduit.Data.Avatar (Avatar, avatarCodec)
import Conduit.Data.Username (Username, usernameCodec)
import Data.Argonaut.Core (jsonFalse)
import Data.Codec ((>~>))
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Migration as CAM
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe)

type ProfileRep r
  = ( username :: Username
    , bio :: Maybe String
    , image :: Maybe Avatar
    | r
    )

type Profile
  = { | ProfileRep ( following :: Boolean ) }

-- | Codecs
profileCodec :: JsonCodec Profile
profileCodec = CAM.addDefaultField "following" jsonFalse >~> codec
  where
  codec =
    CAR.object "Profile"
      { username: usernameCodec
      , bio: CAC.maybe CA.string
      , image: CAC.maybe avatarCodec
      , following: CA.boolean
      }
