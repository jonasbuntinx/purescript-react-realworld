module Conduit.Api.Utils where

import Prelude
import Conduit.Data.Config as Config
import Foreign.Object as Object
import Milkis as Milkis

addBaseUrl :: forall r. { url :: Milkis.URL | r } -> { url :: Milkis.URL | r }
addBaseUrl request@{ url: Milkis.URL url } = request { url = Milkis.URL (Config.apiEndpoint <> url) }

addToken :: forall r. String -> { headers :: Object.Object String | r } -> { headers :: Object.Object String | r }
addToken token request@{ headers } = request { headers = Object.insert "Authorization" ("Token " <> token) headers }
