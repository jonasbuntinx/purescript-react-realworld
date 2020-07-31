module Conduit.Api.Request where

import Prelude
import Apiary.Client (Error(..), makeRequest) as Apiary
import Apiary.Client.Request (class BuildRequest) as Apiary
import Apiary.Client.Response (class DecodeResponse) as Apiary
import Conduit.Api.Utils (addBaseUrl, addToken)
import Conduit.Components.Toast as Toast
import Conduit.Effects.Debug as Debug
import Conduit.Env (Env)
import Control.Comonad (extract)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Bitraversable (lfor)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Effect.Aff (message)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (error)
import Foreign (ForeignError(..), MultipleErrors, renderForeignError)
import Foreign.Object as Object
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
makeRequest route path query body = do
  res <- liftAff $ Apiary.makeRequest route addBaseUrl path query body
  void $ lfor res onError
  pure res

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
  auth <- liftEffect $ (fst <$> read env.userSignal)
  res <- case auth of
    Nothing -> pure $ Left $ Apiary.RuntimeError $ error "Token not available"
    Just { token } -> liftAff $ Apiary.makeRequest route (addBaseUrl <<< addToken token) path query body
  void $ lfor res onError
  pure res

onError :: forall m. MonadEffect m => Apiary.Error -> m Unit
onError error =
  liftEffect do
    case error of
      Apiary.RuntimeError err -> do
        Toast.enqueueToast $ "Runtime error:\n" <> message err
      Apiary.DecodeError req { status, body } errs -> do
        let
          errors = foldl (\obj err -> Object.insert (if err.path == "" then "(root)" else err.path) err.error obj) Object.empty (formatDecodeErrors errs)
        Debug.table "Decode errors" errors
        Toast.enqueueToast $ "Misformatted API response:\n" <> renderForeignError (extract errs)
      Apiary.UnexpectedResponse req { status, body } -> do
        Toast.enqueueToast $ "Unexpected API response (" <> show status <> "):\n" <> body

formatDecodeErrors :: MultipleErrors -> Array { path :: String, error :: String }
formatDecodeErrors = map (flattenError "") <<< NonEmptyList.toUnfoldable
  where
  flattenError :: String -> ForeignError -> { path :: String, error :: String }
  flattenError path (TypeMismatch a b) = { path, error: "Type mismatch: expected " <> a <> ", received " <> b }

  flattenError path (ErrorAtIndex i error) = flattenError (path <> "[" <> show i <> "]") error

  flattenError path (ErrorAtProperty prop error) = flattenError (path <> "." <> prop) error

  flattenError path (ForeignError error) = { path, error }
