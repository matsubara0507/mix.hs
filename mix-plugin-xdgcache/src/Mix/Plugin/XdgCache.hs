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
  , XdgCacheable
  , withCacheOn
  ) where

import           RIO
import           RIO.Directory
import           RIO.FilePath

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
  ) => FilePath -> m Text
readCache filename = do
  cacheDir <- getCacheDirectory
  readFileUtf8 (cacheDir </> filename)

writeCache ::
  ( MonadIO m
  , MonadReader env m
  , HasXdgCacheConfig env
  ) => FilePath -> Text -> m ()
writeCache filename body = do
  cacheDir <- getCacheDirectory
  createDirectoryIfMissing True cacheDir
  writeFileUtf8 (cacheDir </> filename) body

expireCache ::
  ( MonadIO m
  , MonadReader env m
  , HasXdgCacheConfig env
  ) => FilePath -> m ()
expireCache name = do
  cacheName <- (</> name) <$> getCacheDirectory
  whenM (doesFileExist $ cacheName) $ removeFile cacheName

class XdgCacheable a where
  toCache :: a -> Text
  fromCache :: Text -> a

withCacheOn ::
  ( MonadIO m
  , MonadReader env m
  , HasXdgCacheConfig env
  , XdgCacheable a
  ) => m a -> FilePath -> m a
withCacheOn act name = do
  cacheDir <- getCacheDirectory
  isExist  <- doesFileExist (cacheDir </> name)
  if isExist then
    fromCache <$> readCache name
  else do
    result <- act
    writeCache name (toCache result)
    pure result
