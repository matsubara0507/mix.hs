{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import           RIO

import           Data.Extensible
import           Mix
import qualified Mix.Plugin.Logger   as MixLogger
import qualified Mix.Plugin.XdgCache as MixCache

type Env = Record
  '[ "logger"   >: MixLogger.LogFunc
   , "xdgcache" >: MixCache.Config
   ]

main :: IO ()
main = do
  let logConf = #handle @= stdout <: #verbose @= False <: nil
      plugin = hsequence
            $ #logger   <@=> MixLogger.buildPlugin logConf
           <: #xdgcache <@=> MixCache.buildPlugin "mix-plugin-xdgcache-sample"
           <: nil
  Mix.run plugin app

app :: RIO Env ()
app = do
  cacheDir <- MixCache.getCacheDirectory
  logInfo $ fromString ("cache is " ++ cacheDir)
