module Conduit.Data.User where

import Prelude
import Conduit.Data.Profile (ProfileRep, mkProfileRepCodec)
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Type.Proxy (Proxy(..))

type UserRep r =
  (email :: String | ProfileRep r)

type User =
  { | UserRep () }

type CurrentUser =
  { | UserRep (token :: String) }

-- | Codecs
mkUserRepCodec :: forall rest. CA.JPropCodec (Record rest) -> CA.JPropCodec { | UserRep rest }
mkUserRepCodec =
  mkProfileRepCodec
    <<< CA.recordProp (Proxy :: Proxy "email") CA.string

userCodec :: JsonCodec User
userCodec = CA.object "User" $ mkUserRepCodec CA.record

currentUserCodec :: JsonCodec CurrentUser
currentUserCodec = CA.object "User" $ mkUserRepCodec $ CAR.record { token: CA.string }
