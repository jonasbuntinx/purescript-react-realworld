module Conduit.Api.Utils (addBaseUrl, addToken, makeRequest, makeSecureRequest) where

import Prelude
import Apiary as Apiary
import Conduit.Capability.Auth (class MonadAuth, read)
import Conduit.Capability.Routing (class MonadRouting, redirect)
import Conduit.Config as Config
import Conduit.Data.Error (Error(..))
import Conduit.Data.Route (Route(..))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Bitraversable (lfor)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console

makeRequest ::
  forall m rep body query path route response.
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
  forall m rep body query path route response.
  MonadAuth m =>
  MonadRouting Route m =>
  MonadAff m =>
  Apiary.BuildRequest route path query body rep =>
  Apiary.DecodeResponse rep response =>
  route ->
  path ->
  query ->
  body ->
  m (Either Error response)
makeSecureRequest route path query body = do
  auth <- read
  case auth of
    Nothing -> do
      redirect Register
      pure $ Left $ NotAuthorized
    Just { token } -> do
      res <- liftAff $ Apiary.makeRequest route (addBaseUrl <<< addToken token) path query body
      void $ lfor res onError
      pure $ lmap ApiaryError res

addBaseUrl :: forall r. { url :: String | r } -> { url :: String | r }
addBaseUrl request@{ url } = request { url = Config.apiEndpoint <> url }

addToken :: forall r. String -> { headers :: Array Apiary.RequestHeader | r } -> { headers :: Array Apiary.RequestHeader | r }
addToken token request@{ headers } = request { headers = Array.snoc headers (Apiary.RequestHeader "Authorization" ("Token " <> token)) }

onError :: forall m. MonadEffect m => Apiary.Error -> m Unit
onError error = do
  when (Config.nodeEnv /= "production") do
    Console.log $ toLogMessage error
  where
  toLogMessage (Apiary.UnexpectedResponse req { status, body }) = ("Unexpected API response (" <> show status <> "): ") <> body

  toLogMessage err = show err
