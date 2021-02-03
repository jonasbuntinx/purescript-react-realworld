module Main where

import Prelude
import Conduit.Client (client)
import Conduit.Data.Serverless (Context, Event, Response)
import Conduit.Serverless (serverless)
import Control.Promise (Promise, fromAff)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, mkEffectFn2)

main :: Effect Unit
main = client

handler :: EffectFn2 Event Context (Promise Response)
handler = mkEffectFn2 \event context -> fromAff $ serverless event context
