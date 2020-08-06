module Conduit.Effects.Logger.Class where

import Prelude
import Apiary.Types as Apiary
import Control.Comonad (extract)
import Effect.Exception as Exception
import Foreign (renderForeignError)

class Loggable a where
  toLogMessage :: a -> String

instance loggableString :: Loggable String where
  toLogMessage a = a

instance loggableException :: Loggable Exception.Error where
  toLogMessage = Exception.message

instance loggableApiaryError :: Loggable Apiary.Error where
  toLogMessage (Apiary.RuntimeError exc) = "Runtime error: " <> Exception.message exc
  toLogMessage (Apiary.DecodeError req res errs) = "Decode error: " <> (renderForeignError $ extract errs)
  toLogMessage (Apiary.UnexpectedResponse req { status, body }) = ("Unexpected API response (" <> show status <> "): ") <> body
