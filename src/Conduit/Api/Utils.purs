module Conduit.Api.Utils where

import Prelude
import Apiary.Client (Error(..), makeRequest) as Apiary
import Apiary.Client.Request (class BuildRequest) as Apiary
import Apiary.Client.Response (class DecodeResponse) as Apiary
import Conduit.Data.Config as Config
import Conduit.Effects.Debug as Debug
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Bitraversable (lfor)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..))
import Effect.Aff (message)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (error)
import Foreign (ForeignError(..), MultipleErrors)
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
  void $ lfor res onError
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
  void $ lfor res onError
  pure res

addBaseUrl :: forall r. { url :: Milkis.URL | r } -> { url :: Milkis.URL | r }
addBaseUrl request@{ url: Milkis.URL url } = request { url = Milkis.URL (Config.apiEndpoint <> url) }

addToken :: forall r. String -> { headers :: Object.Object String | r } -> { headers :: Object.Object String | r }
addToken token request@{ headers } = request { headers = Object.insert "Authorization" ("Token " <> token) headers }

onError ::
  forall m.
  MonadEffect m =>
  Apiary.Error ->
  m Unit
onError error = do
  case error of
    Apiary.RuntimeError err -> do
      liftEffect $ Debug.log "Runtime error" $ message err
    Apiary.DecodeError req { status, body } errs -> do
      let
        errors = foldl (\obj err -> Object.insert (if err.path == "" then "(root)" else err.path) err.error obj) Object.empty (formatDecodeErrors errs)
      liftEffect $ Debug.table "Misformatted API response" errors
    Apiary.UnexpectedResponse req { status, body } -> do
      liftEffect $ Debug.log ("Unexpected API response (" <> show status <> ")") body

formatDecodeErrors :: MultipleErrors -> Array { path :: String, error :: String }
formatDecodeErrors = map (flattenError "") <<< NonEmptyList.toUnfoldable
  where
  flattenError :: String -> ForeignError -> { path :: String, error :: String }
  flattenError path (TypeMismatch a b) = { path, error: "Type mismatch: expected " <> a <> ", received " <> b }

  flattenError path (ErrorAtIndex i error) = flattenError (path <> "[" <> show i <> "]") error

  flattenError path (ErrorAtProperty prop error) = flattenError (path <> "." <> prop) error

  flattenError path (ForeignError error) = { path, error }
