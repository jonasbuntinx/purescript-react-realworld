module Test.Main where

import Prelude
import Conduit.AppM (AppInstance, AppM)
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Fixture (fixture)

main :: Effect Unit
main = do
  log "🍝"
  log "You should add some tests."

appInstance :: AppInstance AppM
appInstance = fixture
