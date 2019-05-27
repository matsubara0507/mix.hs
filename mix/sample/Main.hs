{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           RIO

import           Data.Extensible
import           Mix
import           Mix.Plugin.Config as MixConfig
import           Mix.Plugin.Logger as MixLogger

type Env = Record
  '[ "logger" >: MixLogger.LogFunc
   , "config" >: Config
   ]

type Config = Record
  '[ "name" >: Text
   ]

main :: IO ()
main = Mix.run plugin $ do
  config <- MixConfig.askConfig
  MixLogger.logDebug $ display ("This is debug: " <> config ^. #name)
  MixLogger.logInfo  $ display ("This is info: "  <> config ^. #name)
  MixLogger.logWarn  $ display ("This is warn: "  <> config ^. #name)
  MixLogger.logError $ display ("This is error: " <> config ^. #name)
  where
    plugin :: Plugin () IO Env
    plugin = hsequence
        $ #logger <@=> MixLogger.buildPlugin (#handle @= stdout <: #verbose @= True <: nil)
       <: #config <@=> MixConfig.buildPlugin (#name @= "Hoge" <: nil)
       <: nil
