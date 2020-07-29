module Conduit.Page.Home where

import Prelude
import Conduit.Component.App as App
import Conduit.Env (Env)
import Effect.Class (liftEffect)
import React.Basic.Hooks as React

data Action
  = NoOp

mkHomePage :: App.Component Env Unit
mkHomePage =
  App.component "HomePage" { init, update } \_ _ props -> React.do
    pure $ render props
  where
  init = {}

  update self = case _ of
    NoOp -> liftEffect $ pure unit

  render props = React.empty
