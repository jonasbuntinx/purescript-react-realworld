module Conduit.Env.Toast where

import Prelude
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Array (snoc)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Wire.React.Class as Wire
import Wire.React.Pure (Pure, create) as Pure

type ToastSignal
  = Pure.Pure (Array { message :: String, duration :: Milliseconds })

create :: Effect ToastSignal
create = Pure.create []

-- | Helpers
toastDuration :: Milliseconds
toastDuration = Milliseconds 4000.0

enqueueToast ::
  forall m r.
  MonadAsk { toastSignal :: ToastSignal | r } m =>
  MonadEffect m =>
  String ->
  m Unit
enqueueToast message = do
  { toastSignal } <- ask
  liftEffect $ Wire.modify toastSignal (_ `snoc` { message, duration: toastDuration })
