module Conduit.Component.Page where

import Prelude
import Conduit.AppM (AppM(..), runAppM)
import Control.Monad.Reader (ask)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import React.Basic.Hooks as React
import React.Halo (liftEffect)
import React.Halo as Halo

type Component props
  = AppM (props -> React.JSX)

component ::
  forall props state action hooks.
  String ->
  { initialState :: state
  , eval :: Halo.Lifecycle props action -> Halo.HaloM props state action AppM Unit
  } ->
  ({ props :: props, state :: state, send :: action -> Effect Unit } -> React.Render (Halo.UseHalo props state action Unit) hooks React.JSX) ->
  Component props
component name { initialState, eval } render = do
  impl <- AppM ask
  liftEffect
    $ React.component name \props -> React.do
        state /\ send <- Halo.useHalo { initialState, props, eval: Halo.hoist (runAppM impl) <<< eval }
        render { props, state, send }
