module Conduit.Capability.Routing where

import Prelude

class Routing route m where
  navigate :: route -> m Unit
  redirect :: route -> m Unit
