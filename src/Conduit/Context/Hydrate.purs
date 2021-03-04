module Conduit.Context.Hydrate where

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

mkHydrateProvider :: Maybe Foreign -> Effect (Context /\ (React.JSX -> React.JSX))
mkHydrateProvider dehydrated = do
  context <- liftEffect $ React.createContext { value: dehydrated, delete: pure unit }
  component <-
    liftEffect
      $ React.component "HydrateProvider " \content -> React.do
          state /\ setState <- React.useState' dehydrated
          pure $ React.provider context { value: state, delete: setState Nothing } $ pure content
  pure (context /\ component)

-- | Hook
newtype UseHydrate hooks
  = UseHydrate
  ( (React.UseEffect Unit)
      ( React.UseContext
          { delete :: Effect Unit
          , value :: Maybe Foreign
          }
          hooks
      )
  )

derive instance newtypeUseHydrate :: Newtype (UseHydrate hooks) _

useHydrate ::
  forall initial hydrated.
  ReadForeign hydrated =>
  Context ->
  initial ->
  (initial -> hydrated -> initial) ->
  React.Hook UseHydrate initial
useHydrate context initial fn =
  React.coerceHook React.do
    { value, delete } <- React.useContext context
    React.useEffectOnce (pure delete)
    case join $ read_ <$> value of
      Nothing -> pure initial
      Just hydrated -> pure $ fn initial hydrated
