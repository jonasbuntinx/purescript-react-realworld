module Conduit.Capability.Halo
  ( class MonadHalo
  , component
  , component_
  , module Halo
  ) where

import Prelude
import React.Halo (JSX, ComponentSpec) as Halo

class
  Monad m <= MonadHalo m where
  component :: forall state action props. String -> Halo.ComponentSpec props state action m -> m (props -> Halo.JSX)

component_ :: forall state action m. MonadHalo m => String -> Halo.ComponentSpec Unit state action m -> m Halo.JSX
component_ name spec = flap (component name spec) unit
