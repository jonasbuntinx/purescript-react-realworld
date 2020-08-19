module Conduit.Env.Routing (RoutingSignal, create, navigate, redirect) where

import Prelude
import Conduit.Data.Route (Route(..), toRouteString)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.NullOrUndefined (undefined)
import Routing.PushState (PushStateInterface)
import Routing.PushState as PushState
import Wire.React.Pure (Pure, create) as Pure

type RoutingSignal
  = Pure.Pure Route

create :: Effect RoutingSignal
create = Pure.create Home

-- | Global
pushStateInterface :: PushStateInterface
pushStateInterface = unsafePerformEffect PushState.makeInterface

-- | Helpers
navigate :: forall m. MonadEffect m => Route -> m Unit
navigate = liftEffect <<< pushStateInterface.pushState undefined <<< toRouteString

redirect :: forall m. MonadEffect m => Route -> m Unit
redirect = liftEffect <<< pushStateInterface.replaceState undefined <<< toRouteString
