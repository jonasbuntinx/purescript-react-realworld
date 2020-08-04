module Conduit.Page.Article where

import Prelude
import Conduit.Component.App as App
import Conduit.Env (Env)
import Effect.Class (liftEffect)
import React.Basic.Hooks as React

data Action
  = NoOp

mkArticlePage :: App.Component Env Unit
mkArticlePage =
  App.component "ArticlePage" { init, update } \_ _ props -> React.do
    pure $ render props
  where
  init = {}

  update self = case _ of
    NoOp -> liftEffect $ pure unit

  render props = React.empty
