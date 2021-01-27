module Conduit.Component.App where

import Prelude
import Conduit.AppM (AppM(..), runAppM)
import Control.Monad.Reader (ask)
import React.Basic.Hooks as React
import React.Halo (liftEffect)
import React.Halo as Halo

type Component props
  = AppM (props -> React.JSX)

component :: forall state action props. String -> Halo.ComponentSpec props state action AppM -> Component props
component name { initialState, eval, render } = do
  inst <- AppM ask
  liftEffect
    $ Halo.component name
        { initialState
        , eval: Halo.hoist (runAppM inst) <<< eval
        , render
        }
