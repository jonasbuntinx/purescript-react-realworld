module Conduit.Api.Client where

import Prelude

import Affjax (defaultRequest)
import Affjax as Affjax
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.ResponseHeader (ResponseHeader)
import Affjax.StatusCode (StatusCode(..))
import Conduit.Api.Endpoint (Endpoint, endpointCodec)
import Conduit.Capability.Auth (class MonadAuth)
import Conduit.Capability.Auth as Auth
import Conduit.Capability.Routing (class MonadRouting)
import Conduit.Capability.Routing as Routing
import Conduit.Config as Config
import Conduit.Data.Route (Route(..))
import Control.Monad.Except (ExceptT(..), except, runExceptT, throwError, withExceptT)
import Data.Argonaut.Core as AC
import Data.Array as Array
import Data.Bitraversable (lfor)
import Data.Codec as Codec
import Data.Codec.Argonaut (JsonDecodeError, JsonCodec, printJsonDecodeError)
import Data.Either (Either(..))
import Data.HTTP.Method (Method)
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationJSON)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Routing.Duplex (print)

type URL =
  String

type Request =
  { method :: Method
  , url :: URL
  , headers :: Array RequestHeader
  , body :: AC.Json
  }

type Response =
  { status :: StatusCode
  , headers :: Array ResponseHeader
  , body :: AC.Json
  }

data Error
  = NotAuthorized
  | RuntimeError Affjax.Error
  | DecodeError Request Response JsonDecodeError
  | UnexpectedResponse Request Response

instance Show Error where
  show NotAuthorized = "(NotAuthorized)"
  show (RuntimeError err) = "(RuntimeError {- " <> Affjax.printError err <> " -})"
  show (DecodeError req res err) = "(DecodeError " <> printRequest req <> " " <> printResponse res <> " " <> printJsonDecodeError err <> ")"
  show (UnexpectedResponse req res) = "(UnexpectedResponse " <> printRequest req <> " " <> printResponse res <> ")"

makeRequest' ::
  forall m body response.
  MonadAff m =>
  Method ->
  StatusCode ->
  Endpoint ->
  (Request -> Request) ->
  JsonCodec body ->
  JsonCodec response ->
  body ->
  m (Either Error response)
makeRequest' method statusCode endpoint transform bodyCodec responseCodec body = liftAff $ runExceptT $ handle =<< fetch request
  where
  request = transform $ buildRequest method endpoint bodyCodec body

  handle resp
    | resp.status == statusCode = decodeResponse resp
    | otherwise = throwError $ UnexpectedResponse request resp

  decodeResponse resp = withExceptT (DecodeError request resp) $ except $ Codec.decode responseCodec resp.body

makeRequest ::
  forall m body response.
  MonadAff m =>
  Method ->
  StatusCode ->
  Endpoint ->
  JsonCodec body ->
  JsonCodec response ->
  body ->
  m (Either Error response)
makeRequest method statusCode endpoint bodyCodec responseCodec body = do
  res <- makeRequest' method statusCode endpoint addBaseUrl bodyCodec responseCodec body
  void $ lfor res onError
  pure res

makeSecureRequest' ::
  forall m body response.
  MonadAff m =>
  String ->
  Method ->
  StatusCode ->
  Endpoint ->
  JsonCodec body ->
  JsonCodec response ->
  body ->
  m (Either Error response)
makeSecureRequest' token method statusCode endpoint bodyCodec responseCodec body = do
  res <- makeRequest' method statusCode endpoint (addBaseUrl <<< addToken token) bodyCodec responseCodec body
  void $ lfor res onError
  pure res

makeSecureRequest ::
  forall m body response.
  MonadAuth m =>
  MonadRouting m =>
  MonadAff m =>
  Method ->
  StatusCode ->
  Endpoint ->
  JsonCodec body ->
  JsonCodec response ->
  body ->
  m (Either Error response)
makeSecureRequest method statusCode endpoint bodyCodec responseCodec body = do
  auth <- Auth.read
  case auth of
    Nothing -> do
      Routing.redirect Register
      pure $ Left $ NotAuthorized
    Just { token } -> do
      makeSecureRequest' token method statusCode endpoint bodyCodec responseCodec body

buildRequest ::
  forall body.
  Method ->
  Endpoint ->
  JsonCodec body ->
  body ->
  Request
buildRequest method endpoint codec body =
  { method
  , url: print endpointCodec endpoint
  , headers: [ ContentType applicationJSON ]
  , body: Codec.encode codec body
  }

fetch :: Request -> ExceptT Error Aff Response
fetch { method, url, headers, body } = do
  response <- withExceptT RuntimeError $ ExceptT runRequest
  pure
    { status: response.status
    , headers: response.headers
    , body: response.body
    }
  where
  runRequest =
    Affjax.request
      $ defaultRequest
          { method = Left method
          , url = url
          , headers = headers
          , responseFormat = ResponseFormat.json
          , content = if AC.isNull body then Nothing else pure $ RequestBody.json body
          }

addBaseUrl :: forall r. { url :: String | r } -> { url :: String | r }
addBaseUrl request@{ url } = request { url = Config.apiEndpoint <> url }

addToken :: forall r. String -> { headers :: Array RequestHeader | r } -> { headers :: Array RequestHeader | r }
addToken token request@{ headers } = request { headers = Array.snoc headers (RequestHeader "Authorization" ("Token " <> token)) }

onError :: forall m. MonadEffect m => Error -> m Unit
onError error = do
  when (Config.nodeEnv /= "production") do
    Console.log $ show error

isNotFound :: forall response. Either Error response -> Boolean
isNotFound = case _ of
  Left (UnexpectedResponse _ { status })
    | status == StatusCode 404 -> true
  _ -> false

isUnprocessableEntity :: forall response. Either Error response -> Boolean
isUnprocessableEntity = case _ of
  Left (UnexpectedResponse _ { status })
    | status == StatusCode 422 -> true
  _ -> false

printRequest :: Request -> String
printRequest req@{ body } = show $ req { body = AC.stringify body }

printResponse :: Response -> String
printResponse res@{ body } = show $ res { body = AC.stringify body }
