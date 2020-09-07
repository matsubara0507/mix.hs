{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Main where

import           RIO

import           Data.Extensible
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Mix
import qualified Mix.Plugin.Logger         as MixLogger
import qualified Mix.Plugin.Persist.Sqlite as MixDB
import           System.Environment        (getEnv)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ToDo
    title String
    done  Bool
|]

type Env = Record
  '[ "logger" >: MixLogger.LogFunc
   , "sqlite" >: MixDB.Config
   ]

main :: IO ()
main = do
  sqlitePath <- liftIO $ fromString <$> getEnv "SQLITE_PATH"
  let logConf = #handle @= stdout <: #verbose @= False <: nil
      plugin = hsequence
            $ #logger <@=> MixLogger.buildPlugin logConf
           <: #sqlite <@=> MixDB.buildPlugin sqlitePath 2
           <: nil
  Mix.run plugin app

app :: RIO Env ()
app = do
  MixDB.runMigrate migrateAll
  MixDB.run $ insert_ (ToDo "Study Haskell!" False)
  enotries <- MixDB.run $ selectList [] []
  MixLogger.logInfo "ToDos: "
  forM_ enotries $ \(Entity _ todo) ->
    if toDoDone todo then
      MixLogger.logInfo (fromString $ "- [x] " ++ toDoTitle todo)
    else
      MixLogger.logInfo (fromString $ "- [ ] " ++ toDoTitle todo)
