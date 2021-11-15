module Conduit.Capability.Halo
  ( class MonadHalo
  , component
  ) where

import Prelude
import React.Basic.Hooks (JSX)
import React.Halo (ComponentSpec)

class
  Monad m <=
  MonadHalo m where
  component :: forall hooks props ctx state action. String -> ComponentSpec hooks props ctx state action m -> m (props -> JSX)
