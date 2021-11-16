module Conduit.Component.ResponseErrors where

import Prelude
import Affjax.StatusCode (StatusCode(..))
import Conduit.Api.Client (Error(..))
import Data.Codec (decode)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Foreign.Object (Object, foldMap)
import Network.RemoteData (RemoteData)
import Network.RemoteData as RemoteData
import React.Basic.DOM as R
import React.Basic.Hooks as React

type ErrorReponse =
  { errors :: Object (Array String) }

responseErrors :: forall a. RemoteData Error a -> React.JSX
responseErrors = case _ of
  RemoteData.Failure (UnexpectedResponse _ { status, body })
    | status == StatusCode 422 ->
        let
          (decodeBody :: Either JsonDecodeError ErrorReponse) = decode errorResponseCodec body
        in
          case decodeBody of
            Left _ ->
              R.ul
                { className: "error-messages"
                , children: [ R.text "Unprocessable entity" ]
                }
            Right { errors } ->
              R.ul
                { className: "error-messages"
                , children: errors # foldMap \key value -> value <#> \error -> R.li_ [ R.text $ key <> " " <> error ]
                }
  RemoteData.Failure (UnexpectedResponse _ { status })
    | status == StatusCode 404 ->
        R.ul
          { className: "error-messages"
          , children: [ R.text "Not found" ]
          }
  RemoteData.Failure NotAuthorized ->
    R.ul
      { className: "error-messages"
      , children: [ R.text "Not authorized" ]
      }
  RemoteData.Failure _ ->
    R.ul
      { className: "error-messages"
      , children: [ R.text "Unknown error: request failed" ]
      }
  _ -> React.empty

-- | Codecs
errorResponseCodec :: JsonCodec ErrorReponse
errorResponseCodec =
  CAR.object "ErrorResponse"
    { errors: CAC.foreignObject $ CA.array CA.string
    }
