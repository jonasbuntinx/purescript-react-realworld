module Conduit.Env.Routing where

import Prelude
import Conduit.Capability.Routing (toRouteURL)
import Conduit.Data.Route (Route(..))
import Data.Either (hush)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Foreign.NullOrUndefined (undefined)
import Routing.Duplex (RouteDuplex', parse)
import Routing.PushState (PushStateInterface)
import Wire.React.Sync (Sync, create) as Sync

type RoutingSignal
  = Sync.Sync { route :: Route, action :: Action }

type RoutingEnv
  = { route :: Route, action :: Action }

data Action
  = NoOp
  | Push
  | Replace

create ::
  PushStateInterface ->
  RouteDuplex' Route ->
  Effect RoutingSignal
create interface routes =
  Sync.create
    { load:
        do
          location <- interface.locationState
          pure
            { route: fromMaybe Error $ hush $ parse routes location.path
            , action: NoOp
            }
    , save:
        \{ route, action } -> case action of
          NoOp -> pure unit
          Push -> interface.pushState undefined $ toRouteURL route
          Replace -> interface.replaceState undefined $ toRouteURL route
    }
