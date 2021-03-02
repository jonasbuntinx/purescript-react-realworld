module Conduit.Context.Hydrate where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import React.Basic.Hooks as React

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
