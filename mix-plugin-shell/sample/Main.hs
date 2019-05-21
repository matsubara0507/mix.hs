{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           RIO

import           Data.Extensible
import           Mix
import qualified Mix.Plugin.Logger as MixLogger
import qualified Mix.Plugin.Shell  as MixShell
import qualified Shelly            as Shell


type Env = Record
  '[ "logger" >: MixLogger.LogFunc
   , "work"   >: FilePath
   ]

main :: IO ()
main = Mix.run plugin $ do
  paths <- MixShell.exec $ Shell.ls "."
  forM_ paths $ MixLogger.logInfo . display . Shell.toTextIgnore
  where
    plugin :: Plugin () IO Env
    plugin = hsequence
        $ #logger <@=> MixLogger.buildPlugin (#handle @= stdout <: #verbose @= True <: nil)
       <: #work   <@=> pure "."
       <: nil
