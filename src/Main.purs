module Main where

import Prelude
import Conduit.Client (client)
import Conduit.Server (server)
import Data.Nullable (Nullable, null)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn3, mkEffectFn3, runEffectFn2)
import Foreign (Foreign)

main :: Effect Unit
main = client

handler ::
  forall r.
  EffectFn3
    { path :: String | r }
    Foreign
    (EffectFn2 (Nullable Foreign) { body :: String, statusCode :: Int } Unit)
    Unit
handler =
  mkEffectFn3 \{ path } _ callback -> do
    server path \body ->
      runEffectFn2 callback null
        { statusCode: 200
        , body
        }
