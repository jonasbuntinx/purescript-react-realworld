module Conduit.Capability.Auth where

import Prelude
import Conduit.Data.Auth (Auth)
import Data.Maybe (Maybe)
import FRP.Event (Event)
import React.Halo (HaloM, lift)

type AuthImpl m
  = { readAuth :: m (Maybe Auth)
    , readAuthEvent :: m (Event (Maybe Auth))
    , modifyAuth :: (Maybe Auth -> Maybe Auth) -> m (Maybe Auth)
    }

class
  Monad m <= MonadAuth m where
  readAuth :: m (Maybe Auth)
  readAuthEvent :: m (Event (Maybe Auth))
  modifyAuth :: (Maybe Auth -> Maybe Auth) -> m (Maybe Auth)

instance monadAuthHaloM :: MonadAuth m => MonadAuth (HaloM props state action m) where
  readAuth = lift readAuth
  readAuthEvent = lift readAuthEvent
  modifyAuth = lift <<< modifyAuth
