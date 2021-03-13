module Conduit.Capability.Auth where

import Prelude
import Conduit.Data.Auth (Auth)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe)
import Halogen.Subscription (Emitter)
import React.Halo (HaloM)

type AuthInstance m
  = { readAuth :: m (Maybe Auth)
    , readAuthEvent :: m (Emitter (Maybe Auth))
    , modifyAuth :: (Maybe Auth -> Maybe Auth) -> m (Maybe Auth)
    }

class
  Monad m <= MonadAuth m where
  readAuth :: m (Maybe Auth)
  readAuthEvent :: m (Emitter (Maybe Auth))
  modifyAuth :: (Maybe Auth -> Maybe Auth) -> m (Maybe Auth)

instance monadAuthHaloM :: MonadAuth m => MonadAuth (HaloM props ctx state action m) where
  readAuth = lift readAuth
  readAuthEvent = lift readAuthEvent
  modifyAuth = lift <<< modifyAuth
