{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mix.Plugin.Process where

import           RIO
import           RIO.Process

import           Data.Extensible
import           Mix.Plugin      (Plugin)

instance Lookup xs "processContext" ProcessContext => HasProcessContext (Record xs) where
  processContextL = lens (view #processContext) (\x y -> x & #processContext `set` y)

buildPlugin :: MonadUnliftIO m => Plugin a m ProcessContext
buildPlugin = mkDefaultProcessContext
