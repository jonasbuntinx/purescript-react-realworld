module Conduit.Component.Auth where

import Prelude
import Conduit.State.Auth (AuthState, create)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import React.Basic.Hooks as React

mkAuthManager ::
  Effect (Tuple AuthState (React.JSX -> React.JSX))
mkAuthManager = do
  authState <- create
  component <-
    React.component "AuthManager" \content -> React.do
      pure content
  pure $ Tuple authState component
