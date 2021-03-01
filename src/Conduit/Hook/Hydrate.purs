module Conduit.Hook.Hydrate where

import Prelude
import Conduit.Context.Hydrate (Context)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Foreign (Foreign)
import React.Basic.Hooks as React
import Simple.JSON (class ReadForeign, read_)

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
