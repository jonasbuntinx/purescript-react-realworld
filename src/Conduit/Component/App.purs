module Conduit.Component.App where

import Prelude
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Effect (Effect)
import Effect.Aff (Aff)
import React.Basic.Hooks as React
import React.Basic.Hooks.Store (Instance, Store, UseStore, useStore)

type Component env props
  = ReaderT env Effect (props -> React.JSX)

component ::
  forall env props state action hooks.
  String ->
  { init :: state
  , update :: Instance props state (ReaderT env Aff) -> action -> ReaderT env Aff Unit
  } ->
  (env -> Store state action -> props -> React.Render (UseStore props state action Unit) hooks React.JSX) ->
  Component env props
component name { init, update } renderFn = do
  env <- ask
  lift
    $ React.component name \props -> React.do
        store <-
          useStore
            { init
            , props
            , update
            , launch: flip runReaderT env
            }
        renderFn env store props

component' ::
  forall env props hooks.
  String ->
  (env -> props -> React.Render Unit hooks React.JSX) ->
  Component env props
component' name renderFn = do
  env <- ask
  lift
    $ React.component name \props -> React.do
        renderFn env props
