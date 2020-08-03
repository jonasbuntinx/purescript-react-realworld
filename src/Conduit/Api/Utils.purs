module Conduit.Api.Utils where

import Prelude
import Apiary.Client (Error(..)) as Apiary
import Conduit.Data.Config as Config
import Conduit.Effects.Debug as Debug
import Conduit.Env.Toast (ToastSignal, enqueueToast)
import Control.Comonad (extract)
import Control.Monad.Reader (class MonadAsk)
import Data.Foldable (foldl)
import Data.List.NonEmpty as NonEmptyList
import Effect.Aff (message)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign (ForeignError(..), MultipleErrors, renderForeignError)
import Foreign.Object as Object
import Milkis as Milkis

addBaseUrl :: forall r. { url :: Milkis.URL | r } -> { url :: Milkis.URL | r }
addBaseUrl request@{ url: Milkis.URL url } = request { url = Milkis.URL (Config.apiEndpoint <> url) }

addToken :: forall r. String -> { headers :: Object.Object String | r } -> { headers :: Object.Object String | r }
addToken token request@{ headers } = request { headers = Object.insert "Authorization" ("Token " <> token) headers }

onError ::
  forall m r.
  MonadAsk { toastSignal :: ToastSignal | r } m =>
  MonadEffect m =>
  Apiary.Error ->
  m Unit
onError error = do
  case error of
    Apiary.RuntimeError err -> do
      enqueueToast $ "Runtime error:\n" <> message err
    Apiary.DecodeError req { status, body } errs -> do
      let
        errors = foldl (\obj err -> Object.insert (if err.path == "" then "(root)" else err.path) err.error obj) Object.empty (formatDecodeErrors errs)
      liftEffect $ Debug.table "Decode errors" errors
      enqueueToast $ "Misformatted API response:\n" <> renderForeignError (extract errs)
    Apiary.UnexpectedResponse req { status, body } -> do
      enqueueToast $ "Unexpected API response (" <> show status <> "):\n" <> body

formatDecodeErrors :: MultipleErrors -> Array { path :: String, error :: String }
formatDecodeErrors = map (flattenError "") <<< NonEmptyList.toUnfoldable
  where
  flattenError :: String -> ForeignError -> { path :: String, error :: String }
  flattenError path (TypeMismatch a b) = { path, error: "Type mismatch: expected " <> a <> ", received " <> b }

  flattenError path (ErrorAtIndex i error) = flattenError (path <> "[" <> show i <> "]") error

  flattenError path (ErrorAtProperty prop error) = flattenError (path <> "." <> prop) error

  flattenError path (ForeignError error) = { path, error }
