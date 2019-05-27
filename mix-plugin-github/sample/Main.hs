{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           RIO

import           Data.Extensible
import qualified GitHub
import qualified GitHub.Endpoints.Users as GitHub
import           Mix
import qualified Mix.Plugin.GitHub      as MixGitHub
import qualified Mix.Plugin.Logger      as MixLogger
import           System.Environment     (getEnv)

type Env = Record
  '[ "logger" >: MixLogger.LogFunc
   , "github" >: MixGitHub.Token
   ]

main :: IO ()
main = do
  gToken <- liftIO $ fromString <$> getEnv "GH_TOKEN"
  let logConf = #handle @= stdout <: #verbose @= False <: nil
      plugin = hsequence
            $ #logger <@=> MixLogger.buildPlugin logConf
           <: #github <@=> MixGitHub.buildPlugin gToken
           <: nil
  Mix.run plugin app

app :: RIO Env ()
app = do
  MixLogger.logInfo "fetch GitHub user info:"
  resp <- MixGitHub.fetch GitHub.userInfoCurrent'
  case resp of
    Left err   -> logError "GitHub fetch error...."
    Right user -> logInfo $ display ("Hi " <> toLogin user <> "!!")
  where
    toLogin = GitHub.untagName . GitHub.userLogin
