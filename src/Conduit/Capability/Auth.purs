module Conduit.Capability.Auth where

import Prelude
import Conduit.Data.Auth (Auth)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe)
import Halogen.Subscription as HS
import React.Halo (HaloM)
import React.Halo as Halo

class
  Monad m <=
  MonadAuth m where
  read :: m (Maybe Auth)
  getEmitter :: m (HS.Emitter (Maybe Auth))
  modify :: (Maybe Auth -> Maybe Auth) -> m (Maybe Auth)

instance MonadAuth m => MonadAuth (HaloM props ctx state action m) where
  read = lift read
  getEmitter = lift getEmitter
  modify = lift <<< modify

subscribe :: forall m props ctx state action. MonadAuth m => (Maybe Auth -> action) -> HaloM props ctx state action m Unit
subscribe f = do
  emitter <- lift getEmitter
  void $ Halo.subscribe $ f <$> emitter
