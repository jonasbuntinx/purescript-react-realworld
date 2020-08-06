module Conduit.Api.Utils where

import Prelude
import Apiary.Client (Error(..), makeRequest) as Apiary
import Apiary.Client.Request (class BuildRequest) as Apiary
import Apiary.Client.Response (class DecodeResponse) as Apiary
import Conduit.Config as Config
import Conduit.Effects.Log as Log
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Bitraversable (lfor)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign.Object as Object
import Milkis as Milkis
import Wire.React.Class (class Atom, read)

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
makeRequest route path query body = do
  res <- liftAff $ Apiary.makeRequest route addBaseUrl path query body
  void $ lfor res Log.debug
  pure res

makeSecureRequest ::
  forall m a r s rep body query path route response.
  MonadAsk { authSignal :: a (Maybe { token :: String | s }) | r } m =>
  Atom a =>
  MonadAff m =>
  Apiary.BuildRequest route path query body rep =>
  Apiary.DecodeResponse rep response =>
  route ->
  path ->
  query ->
  body ->
  m (Either Apiary.Error response)
makeSecureRequest route path query body = do
  env <- ask
  auth <- liftEffect $ read env.authSignal
  res <- case auth of
    Nothing -> pure $ Left $ Apiary.RuntimeError $ error "Token not available"
    Just { token } -> liftAff $ Apiary.makeRequest route (addBaseUrl <<< addToken token) path query body
  void $ lfor res Log.debug
  pure res

addBaseUrl :: forall r. { url :: Milkis.URL | r } -> { url :: Milkis.URL | r }
addBaseUrl request@{ url: Milkis.URL url } = request { url = Milkis.URL (Config.apiEndpoint <> url) }

addToken :: forall r. String -> { headers :: Object.Object String | r } -> { headers :: Object.Object String | r }
addToken token request@{ headers } = request { headers = Object.insert "Authorization" ("Token " <> token) headers }
