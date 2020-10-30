module Conduit.Component.Store where

import Prelude
import Conduit.Component.Env as Env
import Conduit.Data.Env (Env)
import Conduit.StoreM (StoreM, runStoreM)
import Control.Monad.Reader (ask)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import React.Basic.Hooks as React
import React.Halo as Halo

type Component props
  = Env.Component props

component ::
  forall props state action hooks.
  String ->
  { initialState :: state
  , update :: { props :: props, state :: state } -> action -> Halo.HaloM props state action (StoreM Aff) Unit
  } ->
  (Env -> { state :: state, send :: action -> Effect Unit } -> props -> React.Render (Halo.UseHalo props state action Unit) hooks React.JSX) ->
  Env.Component props
component name { initialState, update } renderFn = do
  env <- ask
  liftEffect
    $ React.component name \props -> React.do
        state /\ send <-
          Halo.useHalo
            { initialState
            , props
            , eval:
                Halo.hoist (runStoreM env)
                  <<< Halo.makeEval
                      _
                        { onAction =
                          \action -> do
                            state <- Halo.get
                            props' <- Halo.props
                            update { props: props', state } action
                        }
            }
        renderFn env { state, send } props
