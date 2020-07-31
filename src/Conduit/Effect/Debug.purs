module Conduit.Effects.Debug where

import Prelude
import Effect (Effect)

foreign import log :: forall a. String -> a -> Effect Unit

foreign import table :: forall a. String -> a -> Effect Unit
