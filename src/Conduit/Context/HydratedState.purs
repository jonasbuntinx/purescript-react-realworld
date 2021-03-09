module Conduit.Context.HydratedState where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import React.Basic.Hooks as React
import Simple.JSON (class ReadForeign, read_)

-- | Provider
type Context
  = React.ReactContext { value :: Maybe Foreign, delete :: Effect Unit }

mkHydratedStateProvider :: Maybe Foreign -> Effect (Context /\ (React.JSX -> React.JSX))
mkHydratedStateProvider dehydrated = do
  context <- liftEffect $ React.createContext { value: dehydrated, delete: pure unit }
  component <-
    liftEffect
      $ React.component "HydratedStateProvider " \content -> React.do
          state /\ setState <- React.useState' dehydrated
          pure $ React.provider context { value: state, delete: setState Nothing } $ pure content
  pure (context /\ component)

-- | Hook
newtype UseHydratedState hooks
  = UseHydrate
  ( (React.UseEffect Unit)
      ( React.UseContext
          { delete :: Effect Unit
          , value :: Maybe Foreign
          }
          hooks
      )
  )

derive instance newtypeUseHydrate :: Newtype (UseHydratedState hooks) _

useHydratedState ::
  forall initial hydrated.
  ReadForeign hydrated =>
  Context ->
  initial ->
  (initial -> hydrated -> initial) ->
  React.Hook UseHydratedState initial
useHydratedState context initial fn =
  React.coerceHook React.do
    { value, delete } <- React.useContext context
    React.useEffectOnce (pure delete)
    case join $ read_ <$> value of
      Nothing -> pure initial
      Just hydrated -> pure $ fn initial hydrated
