module Conduit.Api.Request where

import Prelude
import Apiary.Client (Error, makeRequest) as Apiary
import Apiary.Client.Request (class BuildRequest) as Apiary
import Apiary.Client.Response (class DecodeResponse) as Apiary
import Conduit.Api.Utils (addBaseUrl, addToken)
import Conduit.Env (Env)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Either (Either)
import Data.Maybe (maybe')
import Data.Tuple (fst)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Wire.React.Class (read)

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
  forall m route path query body rep response.
  MonadAff m =>
  MonadAsk Env m =>
  Apiary.BuildRequest route path query body rep =>
  Apiary.DecodeResponse rep response =>
  route ->
  path ->
  query ->
  body ->
  m (Either Apiary.Error response)
makeSecureRequest route path query body = do
  env <- ask
  { token } <- liftEffect $ (fst <$> read env.userSignal) >>= maybe' (const $ throw "Token not available") pure
  liftAff $ Apiary.makeRequest route (addBaseUrl <<< addToken token) path query body
