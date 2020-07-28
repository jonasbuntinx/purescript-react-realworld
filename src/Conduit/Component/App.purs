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
  (props -> Store state action -> React.Render (UseStore props state action Unit) hooks React.JSX) ->
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
        renderFn props store
