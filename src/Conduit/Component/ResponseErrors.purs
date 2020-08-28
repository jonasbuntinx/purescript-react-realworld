module Conduit.Component.ResponseErrors where

import Prelude
import Conduit.Data.Error (Error(..))
import Foreign.Object (foldMap)
import Network.RemoteData (RemoteData)
import Network.RemoteData as RemoteData
import React.Basic.DOM as R
import React.Basic.Hooks as React

responseErrors :: forall a. RemoteData Error a -> React.JSX
responseErrors = case _ of
  RemoteData.Failure (UnprocessableEntity err) ->
    R.ul
      { className: "error-messages"
      , children: err # foldMap \key value -> value <#> \error -> R.li_ [ R.text $ key <> " " <> error ]
      }
  RemoteData.Failure (NotFound err) ->
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
