module Main where

import Prelude
import Effect (Effect)
import Entries.Client as Client
import Entries.Serverless as Serverless

main :: Effect Unit
main = Client.main

handler :: Serverless.Handler
handler = Serverless.handler
