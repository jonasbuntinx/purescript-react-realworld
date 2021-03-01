module Conduit.Component.App where

import Prelude
import Conduit.AppM (AppM(..), runAppM)
import Conduit.Context.Hydrate (Context)
import Conduit.Hook.Hydrate (useHydrate)
import Control.Monad.Reader (ask)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import React.Basic.Hooks as React
import React.Halo (liftEffect)
import React.Halo as Halo
import Simple.JSON (class ReadForeign)

type Component props
  = AppM (props -> React.JSX)

component :: forall state action props. String -> Halo.ComponentSpec props state action AppM -> Component props
component name spec =
  AppM do
    impl <- ask
    liftEffect
      $ Halo.component name
          spec { eval = Halo.hoist (runAppM impl) <<< spec.eval }

component' ::
  forall state hydrated action props.
  ReadForeign hydrated =>
  String ->
  Context ->
  { initialState :: state
  , hydrate :: state -> hydrated -> state
  , eval :: Halo.Lifecycle props action -> Halo.HaloM props state action AppM Unit
  , render ::
      { props :: props
      , state :: state
      , send :: action -> Effect Unit
      } ->
      React.JSX
  } ->
  Component props
component' name context spec =
  AppM do
    impl <- ask
    liftEffect
      $ React.component name \props -> React.do
          initialState <- useHydrate context spec.initialState spec.hydrate
          state /\ send <- Halo.useHalo { props, initialState, eval: Halo.hoist (runAppM impl) <<< spec.eval }
          pure (spec.render { props, state, send })
