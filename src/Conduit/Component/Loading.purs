module Conduit.Component.Loading where

import Prelude
import Data.Monoid (guard)
import Data.Tuple.Nested ((/\))
import Effect.Timer as Timer
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.DOM as R
import React.Basic.Hooks as React

loading :: React.JSX
loading = (unsafePerformEffect mkLoading) unit

mkLoading :: React.Component Unit
mkLoading = do
  React.component "Loading" \_ -> React.do
    visible /\ setVisible <- React.useState false
    React.useEffectOnce do
      timeoutId <- Timer.setTimeout 400 do setVisible $ const true
      pure $ Timer.clearTimeout timeoutId
    pure $ guard visible $ R.text "Loading..."
