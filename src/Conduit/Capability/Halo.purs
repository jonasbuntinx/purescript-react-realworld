module Conduit.Capability.Halo where

import Prelude
import React.Halo (JSX, ComponentSpec)

class
  Monad m <= MonadHalo m where
  component :: forall state action props. String -> ComponentSpec props state action m -> m (props -> JSX)
