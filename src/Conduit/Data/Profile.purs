module Conduit.Data.Profile where

import Prelude
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
import Type.Proxy (Proxy(..))

type ProfileRep r =
  ( username :: Username
  , bio :: Maybe String
  , image :: Maybe Avatar
  | r
  )

type Profile =
  { | ProfileRep (following :: Boolean) }

-- | Codecs
mkProfileRepCodec :: forall rest. CA.JPropCodec (Record rest) -> CA.JPropCodec { | ProfileRep rest }
mkProfileRepCodec =
  CA.recordProp (Proxy :: Proxy "username") usernameCodec
    <<< CA.recordProp (Proxy :: Proxy "bio") (CAC.maybe CA.string)
    <<< CA.recordProp (Proxy :: Proxy "image") (CAC.maybe avatarCodec)

profileCodec :: JsonCodec Profile
profileCodec = CAM.addDefaultField "following" jsonFalse >~> codec
  where
  codec = CA.object "Profile" $ mkProfileRepCodec $ CAR.record { following: CA.boolean }
