module Conduit.Effects.Log where

import Prelude
import Conduit.Config as Config
import Conduit.Effects.Logger.Class (class Loggable, toLogMessage)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Foreign.Moment (Format(..), format, now)

data LogLevel
  = Error
  | Warn
  | Info
  | Debug

log :: forall m a. MonadEffect m => Loggable a => LogLevel -> a -> m Unit
log level a = do
  now <- liftEffect now
  let
    headerWith start = "[" <> start <> ": " <> format (Format "YYYY-MM-DD HH:mm:ss") now <> "]\n" <> toLogMessage a

    formattedLogMessage = case level of
      Error -> headerWith "ERROR"
      Warn -> headerWith "WARNING"
      Info -> headerWith "INFO"
      Debug -> headerWith "DEBUG"
  case Config.nodeEnv, level of
    "production", Debug -> pure unit
    _, _ -> Console.log formattedLogMessage

error :: forall a m. MonadEffect m => Loggable a => a -> m Unit
error = log Error

warn :: forall a m. MonadEffect m => Loggable a => a -> m Unit
warn = log Warn

info :: forall a m. MonadEffect m => Loggable a => a -> m Unit
info = log Info

debug :: forall a m. MonadEffect m => Loggable a => a -> m Unit
debug = log Debug
