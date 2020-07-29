module Conduit.Api.Utils where

import Prelude
import Apiary.Client (Error, makeRequest) as Apiary
import Apiary.Client.Request (class BuildRequest) as Apiary
import Apiary.Client.Response (class DecodeResponse) as Apiary
import Conduit.Data.Config as Config
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Either (Either)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Foreign.Object as Object
import Milkis as Milkis

makeRequest ::
  forall m route path query body rep response.
  MonadAff m =>
  Apiary.BuildRequest route path query body rep =>
  Apiary.DecodeResponse rep response =>
  route ->
  path ->
  query ->
  body ->
  m (Either Apiary.Error response)
makeRequest route path query body = liftAff $ Apiary.makeRequest route addBaseUrl path query body

makeSecureRequest ::
  forall m r route path query body rep response.
  MonadAff m =>
  MonadAsk { token :: String | r } m =>
  Apiary.BuildRequest route path query body rep =>
  Apiary.DecodeResponse rep response =>
  route ->
  path ->
  query ->
  body ->
  m (Either Apiary.Error response)
makeSecureRequest route path query body = do
  { token } <- ask
  liftAff $ makeSecureRequest' token route path query body

makeSecureRequest' ::
  forall route path query body rep response.
  Apiary.BuildRequest route path query body rep =>
  Apiary.DecodeResponse rep response =>
  String ->
  route ->
  path ->
  query ->
  body ->
  Aff (Either Apiary.Error response)
makeSecureRequest' token route path query body = do
  Apiary.makeRequest route (addBaseUrl <<< addToken token) path query body

-- | Helpers
addBaseUrl :: forall r. { url :: Milkis.URL | r } -> { url :: Milkis.URL | r }
addBaseUrl request@{ url: Milkis.URL url } = request { url = Milkis.URL (Config.apiEndpoint <> url) }

addToken :: forall r. String -> { headers :: Object.Object String | r } -> { headers :: Object.Object String | r }
addToken token request@{ headers } = request { headers = Object.insert "Authorization" ("Token " <> token) headers }
