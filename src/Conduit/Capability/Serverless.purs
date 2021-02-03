module Conduit.Capability.Serverless where

import Prelude
import Conduit.Data.Serverless (Event, Context)

type ServerlessInstance m
  = { getStateBuilder :: m (StateBuilder m)
    }

class
  Monad m <= MonadServerless m where
  buildInitialState :: forall s. StateBuilder' m s

type StateBuilder' m s
  = m s -> (Event -> Context -> m s) -> m s

newtype StateBuilder m
  = StateBuilder (forall s. StateBuilder' m s)

mkStateBuilder :: forall m. Monad m => (forall s. StateBuilder' m s) -> m (StateBuilder m)
mkStateBuilder sb = pure (StateBuilder sb)

runStateBuilder :: forall m. Monad m => m (StateBuilder m) -> (forall s. StateBuilder' m s)
runStateBuilder msb ms fms = do
  sb <- msb
  case sb of StateBuilder f -> f ms fms
