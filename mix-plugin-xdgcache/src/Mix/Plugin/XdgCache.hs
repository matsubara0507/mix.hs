{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Mix.Plugin.XdgCache
  ( Config
  , buildPlugin
  , HasXdgCacheConfig (..)
  , getCacheDirectory
  , readCache
  , writeCache
  , expireCache
  , withCacheOn
  , JsonParseError (..)
  ) where

import           RIO
import           RIO.Directory
import           RIO.FilePath

import           Data.Aeson      (FromJSON, ToJSON)
import qualified Data.Aeson      as JSON
import           Data.Extensible
import           Mix.Plugin      (Plugin, toPlugin)

type Config = FilePath

buildPlugin :: FilePath -> Plugin a m Config
buildPlugin dirName = toPlugin $ \f -> f dirName

class HasXdgCacheConfig env where
  configL :: Lens' env Config

instance Lookup xs "xdgcache" Config => HasXdgCacheConfig (Record xs) where
  configL = lens (view #xdgcache) (\x y -> x & #xdgcache `set` y)

getCacheDirectory ::
  ( MonadIO m
  , MonadReader env m
  , HasXdgCacheConfig env
  ) => m FilePath
getCacheDirectory = getXdgDirectory XdgCache =<< view configL

readCache ::
  ( MonadIO m
  , MonadReader env m
  , HasXdgCacheConfig env
  ) => FilePath -> m ByteString
readCache filename = do
  cacheDir <- getCacheDirectory
  readFileBinary (cacheDir </> filename)

writeCache ::
  ( MonadIO m
  , MonadReader env m
  , HasXdgCacheConfig env
  ) => FilePath -> ByteString -> m ()
writeCache filename body = do
  cacheDir <- getCacheDirectory
  createDirectoryIfMissing True cacheDir
  writeFileBinary (cacheDir </> filename) body

expireCache ::
  ( MonadIO m
  , MonadReader env m
  , HasXdgCacheConfig env
  ) => FilePath -> m ()
expireCache name = do
  cacheName <- (</> name) <$> getCacheDirectory
  whenM (doesFileExist $ cacheName) $ removeFile cacheName

withCacheOn ::
  ( MonadIO m
  , MonadReader env m
  , HasXdgCacheConfig env
  , FromJSON a
  , ToJSON a
  ) => m a -> FilePath -> m a
withCacheOn act name = do
  cacheDir <- getCacheDirectory
  isExist  <- doesFileExist (cacheDir </> name)
  if isExist then do
    result <- JSON.eitherDecodeStrict <$> readCache name
    case result of
      Left e  -> throwIO (JsonParseError e)
      Right a -> pure a
  else do
    createDirectoryIfMissing True cacheDir
    result <- act
    liftIO $ JSON.encodeFile (cacheDir </> name) result
    pure result

newtype JsonParseError = JsonParseError String deriving Show

instance Exception JsonParseError
