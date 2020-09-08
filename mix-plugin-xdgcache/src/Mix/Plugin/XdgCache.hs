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
  , readCacheByName
  , writeCacheToName
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

readCacheByName ::
  ( MonadIO m
  , MonadReader env m
  , HasXdgCacheConfig env
  ) => FilePath -> m Text
readCacheByName filename = do
  cacheDir <- getCacheDirectory
  readFileUtf8 (cacheDir </> filename)

writeCacheToName ::
  ( MonadIO m
  , MonadReader env m
  , HasXdgCacheConfig env
  ) => FilePath -> Text -> m ()
writeCacheToName filename body = do
  cacheDir <- getCacheDirectory
  createDirectoryIfMissing True cacheDir
  writeFileUtf8 (cacheDir </> filename) body
