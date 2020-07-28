module Conduit.Effects.Routing where

import Prelude
import Control.Monad.Error.Class (try)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.NullOrUndefined (undefined)
import Global.Unsafe (unsafeDecodeURIComponent)
import Routing as Routing
import Routing.Duplex (RouteDuplex', parse)
import Routing.Match (Match)
import Routing.PushState (PushStateInterface)
import Routing.PushState as PushState
import Wire.Event (Event)
import Wire.Event as Event

pushStateInterface :: PushStateInterface
pushStateInterface = unsafePerformEffect PushState.makeInterface

onPushState :: forall a. RouteDuplex' a -> Event (Maybe a /\ a)
onPushState matcher = Event.makeEvent \k -> PushState.matchesWith (parse matcher) (\old new -> k (old /\ new)) pushStateInterface

stripHash :: forall a. Match a -> String -> Either String a
stripHash = Routing.matchWith (unsafeDecodeURIComponent <<< String.takeWhile (_ /= String.codePointFromChar '#'))

pushState :: String -> Effect Unit
pushState url = void $ try $ pushStateInterface.pushState undefined url

replaceState :: String -> Effect Unit
replaceState url = void $ try $ pushStateInterface.replaceState undefined url

class HasRoute a where
  toRouteString :: a -> String

navigate :: forall m a. MonadEffect m => HasRoute a => a -> m Unit
navigate = liftEffect <<< pushState <<< toRouteString

redirect :: forall m a. MonadEffect m => HasRoute a => a -> m Unit
redirect = liftEffect <<< replaceState <<< toRouteString
