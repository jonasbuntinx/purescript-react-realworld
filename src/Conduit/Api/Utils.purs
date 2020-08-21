module Conduit.Api.Utils (Error(..), addBaseUrl, addToken, makeRequest, makeSecureRequest) where

import Prelude
import Apiary.Client (makeRequest) as Apiary
import Apiary.Client.Request (class BuildRequest) as Apiary
import Apiary.Client.Response (class DecodeResponse) as Apiary
import Apiary.Types (Error(..)) as Apiary
import Conduit.Config as Config
import Conduit.Data.Auth (Auth)
import Conduit.Data.Route (Route(..))
import Control.Comonad (extract)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Bifunctor (lmap)
import Data.Bitraversable (lfor)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Foreign (renderForeignError)
import Foreign.Object as Object
import Milkis as Milkis

data Error
  = NotAuthorized
  | ApiaryError Apiary.Error

makeRequest ::
  forall m route path query body rep response.
  MonadAff m =>
  Apiary.BuildRequest route path query body rep =>
  Apiary.DecodeResponse rep response =>
  route ->
  path ->
  query ->
  body ->
  m (Either Error response)
makeRequest route path query body = do
  res <- liftAff $ Apiary.makeRequest route addBaseUrl path query body
  void $ lfor res onError
  pure $ lmap ApiaryError res

makeSecureRequest ::
  forall m r rep body query path route response.
  MonadAsk { readAuth :: Effect (Maybe Auth), redirect :: Route -> Effect Unit | r } m =>
  MonadAff m =>
  Apiary.BuildRequest route path query body rep =>
  Apiary.DecodeResponse rep response =>
  route ->
  path ->
  query ->
  body ->
  m (Either Error response)
makeSecureRequest route path query body = do
  env <- ask
  auth <- liftEffect $ env.readAuth
  case auth of
    Nothing -> do
      liftEffect $ env.redirect Register
      pure $ Left $ NotAuthorized
    Just { token } -> do
      res <- liftAff $ Apiary.makeRequest route (addBaseUrl <<< addToken token) path query body
      void $ lfor res onError
      pure $ lmap ApiaryError res

addBaseUrl :: forall r. { url :: Milkis.URL | r } -> { url :: Milkis.URL | r }
addBaseUrl request@{ url: Milkis.URL url } = request { url = Milkis.URL (Config.apiEndpoint <> url) }

addToken :: forall r. String -> { headers :: Object.Object String | r } -> { headers :: Object.Object String | r }
addToken token request@{ headers } = request { headers = Object.insert "Authorization" ("Token " <> token) headers }

onError :: forall m. MonadEffect m => Apiary.Error -> m Unit
onError error = do
  when (Config.nodeEnv /= "production") do
    Console.log $ toLogMessage error
  where
  toLogMessage (Apiary.RuntimeError exc) = "Runtime error: " <> Exception.message exc

  toLogMessage (Apiary.DecodeError req res errs) = "Decode error: " <> (renderForeignError $ extract errs)

  toLogMessage (Apiary.UnexpectedResponse req { status, body }) = ("Unexpected API response (" <> show status <> "): ") <> body
