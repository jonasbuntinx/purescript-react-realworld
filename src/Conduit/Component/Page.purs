module Conduit.Component.Page where

import Prelude
import Conduit.AppM (AppM, runAppM)
import Conduit.Component.Env as Env
import Conduit.Data.Env (Env)
import Control.Monad.Reader (ask)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import React.Basic.Hooks as React
import React.Halo as Halo

type Component props
  = Env.Component props

component ::
  forall props state action hooks.
  String ->
  { initialState :: state
  , eval :: Halo.Lifecycle props action -> Halo.HaloM props state action AppM Unit
  } ->
  ({ env :: Env, props :: props, state :: state, send :: action -> Effect Unit } -> React.Render (Halo.UseHalo props state action Unit) hooks React.JSX) ->
  Env.Component props
component name { initialState, eval } render = do
  env <- ask
  liftEffect
    $ React.component name \props -> React.do
        state /\ send <- Halo.useHalo { initialState, props, eval: Halo.hoist (runAppM env) <<< eval }
        render { env, props, state, send }
