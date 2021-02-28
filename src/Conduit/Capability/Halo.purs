module Conduit.Capability.Halo
  ( class MonadHalo
  , component
  , module Halo
  ) where

import Prelude
import React.Halo (JSX, ComponentSpec) as Halo

class
  Monad m <= MonadHalo m where
  component :: forall state action props. String -> Halo.ComponentSpec props state action m -> m (props -> Halo.JSX)
