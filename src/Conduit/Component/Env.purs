module Conduit.Component.Env where

import Prelude
import Conduit.Data.Env (Env)
import Conduit.StoreM (StoreM, runStoreM)
import Control.Monad.Reader (ReaderT, ask)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import React.Basic.Hooks as React
import React.Store (Instance, Store, UseStore, useStore)

type Component props
  = ReaderT Env Effect (props -> React.JSX)

component ::
  forall props state action hooks.
  String ->
  { init :: state
  , update :: Instance props state (StoreM Aff) -> action -> StoreM Aff Unit
  } ->
  (Env -> Store state action -> props -> React.Render (UseStore props state action (StoreM Aff) Unit) hooks React.JSX) ->
  Component props
component name { init, update } renderFn = do
  env <- ask
  liftEffect
    $ React.component name \props -> React.do
        store <- useStore { init, props, update, launch: runStoreM env }
        renderFn env store props

component' ::
  forall props hooks.
  String ->
  (Env -> props -> React.Render Unit hooks React.JSX) ->
  Component props
component' name renderFn = do
  env <- ask
  liftEffect
    $ React.component name \props -> React.do
        renderFn env props
