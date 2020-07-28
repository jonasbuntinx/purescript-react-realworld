module Conduit.Page.Home where

import Prelude
import Conduit.Component.App as App
import Effect.Class (liftEffect)
import React.Basic.Hooks as React

data Action
  = NoOp

mkHomePage :: forall env. App.Component env Unit
mkHomePage =
  App.component "HomePage" { init, update } \props store -> React.do
    pure $ render props store
  where
  init = {}

  update self = case _ of
    NoOp -> liftEffect $ pure unit

  render props store = React.empty
