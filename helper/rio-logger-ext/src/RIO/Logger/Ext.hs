{-# LANGUAGE LambdaCase #-}

module RIO.Logger.Ext
    ( L.runLoggingT
    , askMonadLogger
    ) where

import           RIO

import qualified Control.Monad.Logger  as L
import           System.Log.FastLogger (fromLogStr)

askMonadLogger ::
  ( HasLogFunc env
  , MonadReader env m
  , MonadUnliftIO m
  ) => m (L.Loc -> L.LogSource -> L.LogLevel -> L.LogStr -> IO ())
askMonadLogger = do
  unliift <- askRunInIO
  pure $ \_ source level msg -> unliift $
    logGeneric
      source
      (toLogLevel level)
      (displayBytesUtf8 $ fromLogStr msg)
  where
    toLogLevel = \case
      L.LevelDebug     -> LevelDebug
      L.LevelInfo      -> LevelInfo
      L.LevelWarn      -> LevelWarn
      L.LevelError     -> LevelError
      L.LevelOther txt -> LevelOther txt
