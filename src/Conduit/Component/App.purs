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
component name spec =
  AppM do
    impl <- ask
    liftEffect
      $ Halo.component name
          spec { eval = Halo.hoist (runAppM impl) <<< spec.eval }
