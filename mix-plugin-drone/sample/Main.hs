{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           RIO

import           Data.Extensible
import qualified Drone
import           Mix
import qualified Mix.Plugin.Drone   as MixDrone
import qualified Mix.Plugin.Logger  as MixLogger
import           Network.HTTP.Req   (responseBody)
import           System.Environment (getEnv)

type Env = Record
  '[ "logger" >: MixLogger.LogFunc
   , "drone"  >: MixDrone.Config
   ]

main :: IO ()
main = do
  dHost  <- liftIO $ fromString <$> getEnv "DRONE_HOST"
  dToken <- liftIO $ fromString <$> getEnv "DRONE_TOKEN"
  let logConf = #handle @= stdout <: #verbose @= False <: nil
      dClient = #host @= dHost <: #port @= Nothing <: #token @= dToken <: nil
      plugin = hsequence
            $ #logger <@=> MixLogger.buildPlugin logConf
           <: #drone  <@=> MixDrone.buildPlugin dClient True
           <: nil
  Mix.run plugin app

app :: RIO Env ()
app = do
  MixLogger.logInfo "fetch Drone user info:"
  tryAny (responseBody <$> MixDrone.fetch Drone.getSelf) >>= \case
    Left err   -> logError "Drone CI fetch error..."
    Right user -> logInfo $ display ("Hi " <> user ^. #login <> "!!")
