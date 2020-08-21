module Conduit.Env.Routing where

import Prelude
import Conduit.Data.Route (Route(..))
import Control.Monad.Reader (class MonadAsk, ask)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Wire.React.Pure (Pure, create) as Pure

type RoutingSignal
  = Pure.Pure Route

create :: Effect RoutingSignal
create = Pure.create Home

-- | Helpers
navigate :: forall m r. MonadAsk { navigate :: Route -> Effect Unit | r } m => MonadEffect m => Route -> m Unit
navigate route = ask >>= \env -> liftEffect $ env.navigate route

redirect :: forall m r. MonadAsk { redirect :: Route -> Effect Unit | r } m => MonadEffect m => Route -> m Unit
redirect route = ask >>= \env -> liftEffect $ env.redirect route
