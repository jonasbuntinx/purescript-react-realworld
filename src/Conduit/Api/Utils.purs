module Conduit.Api.Utils where

import Prelude
import Apiary.Client (Error, makeRequest) as Apiary
import Apiary.Client.Request (class BuildRequest) as Apiary
import Apiary.Client.Response (class DecodeResponse) as Apiary
import Conduit.Data.Config as Config
import Data.Either (Either)
import Effect.Aff.Class (class MonadAff, liftAff)
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

-- | Helpers
addBaseUrl :: forall r. { url :: Milkis.URL | r } -> { url :: Milkis.URL | r }
addBaseUrl request@{ url: Milkis.URL url } = request { url = Milkis.URL (Config.apiEndpoint <> url) }
