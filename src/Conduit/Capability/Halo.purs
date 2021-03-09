module Conduit.Capability.Halo
  ( class MonadHalo
  , component
  , component'
  , module Halo
  ) where

import Prelude
import Conduit.Context.HydratedState (Context)
import Effect (Effect)
import React.Halo (JSX, ComponentSpec, HaloM, Lifecycle) as Halo
import Simple.JSON (class ReadForeign)

class
  Monad m <= MonadHalo m where
  component :: forall state action props. String -> Halo.ComponentSpec props state action m -> m (props -> Halo.JSX)
  component' ::
    forall state hydrated action props.
    ReadForeign hydrated =>
    Context ->
    String ->
    { initialState :: state
    , hydrateState :: state -> hydrated -> state
    , eval :: Halo.Lifecycle props action -> Halo.HaloM props state action m Unit
    , render ::
        { props :: props
        , state :: state
        , send :: action -> Effect Unit
        } ->
        Halo.JSX
    } ->
    m (props -> Halo.JSX)
