module Conduit.Capability.Routing where

import Prelude

class IsRoute route where
  toRouteURL :: route -> String

class Routing route m where
  navigate :: IsRoute route => route -> m Unit
  redirect :: IsRoute route => route -> m Unit
