{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Mix.Plugin.Drone
  ( HasDroneClient (..)
  , Config (..)
  , ConfigR
  , buildPlugin
  , fetch
  ) where

import           RIO

import           Data.Extensible
import           Drone
import           Mix.Plugin       (Plugin, toPlugin)
import           Network.HTTP.Req (Req, defaultHttpConfig, runReq)

newtype Config = Config ConfigR

type ConfigR = Record
  '[ "base"    >: Drone.BaseClient
   , "is_http" >: Bool
   ]

buildPlugin :: MonadIO m => Drone.BaseClient -> Bool -> Plugin a m Config
buildPlugin base isHttp =
  toPlugin $ \f -> f (Config $ #base @= base <: #is_http @= isHttp <: nil)

class HasDroneClient env where
  clientL :: Lens' env Config

instance Lookup xs "drone" Config => HasDroneClient (Record xs) where
  clientL = lens (view #drone) (\x y -> x & #drone `set` y)

fetch ::
  ( MonadIO m
  , MonadReader env m
  , HasDroneClient env
  ) => (forall c . Drone.Client c => c -> Req a) -> m a
fetch req = do
  (Config config) <- view clientL
  if config ^. #is_http then
    runReq defaultHttpConfig $ req (Drone.HttpClient $ config ^. #base)
  else
    runReq defaultHttpConfig $ req (Drone.HttpsClient $ config ^. #base)
