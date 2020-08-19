module Conduit.Component.ResponseErrors where

import Prelude
import Foreign.Object (Object, foldMap)
import Network.RemoteData (RemoteData)
import Network.RemoteData as RemoteData
import React.Basic.DOM as R
import React.Basic.Hooks as React

responseErrors :: forall a. RemoteData (Object (Array String)) a -> React.JSX
responseErrors = case _ of
  RemoteData.Failure submissionErrors ->
    R.ul
      { className: "error-messages"
      , children:
          submissionErrors
            # foldMap \key value -> value <#> \error -> R.li_ [ R.text $ key <> " " <> error ]
      }
  _ -> React.empty
