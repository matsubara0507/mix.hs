{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Mix.Plugin.GitHub
  ( HasGitHubToken (..)
  , buildPlugin
  , tokenText
  , auth
  , fetch
  , Token
  ) where

import           RIO

import           Data.Extensible
import qualified GitHub.Auth             as GitHub
import qualified GitHub.Data.Definitions as GitHub
import           GitHub.Request          (GenRequest, ParseResponse, github)
import           Mix.Plugin              (Plugin, toPlugin)

type Token = ByteString

buildPlugin ::  MonadIO m => Token -> Plugin a m Token
buildPlugin token = toPlugin $ \f -> f token

class HasGitHubToken env where
  tokenL :: Lens' env Token

instance Lookup xs "github" Token => HasGitHubToken (Record xs) where
  tokenL = lens (view #github) (\x y -> x & #github `set` y)

tokenText :: (MonadIO m, MonadReader env m, HasGitHubToken env) => m Text
tokenText = decodeUtf8With lenientDecode <$> view tokenL

auth :: (MonadIO m, MonadReader env m, HasGitHubToken env) => m GitHub.Auth
auth = GitHub.OAuth <$> view tokenL

fetch
  :: (MonadIO m, MonadReader env m, HasGitHubToken env, ParseResponse mt a)
  => GenRequest mt rw a -> m (Either GitHub.Error a)
fetch req = (liftIO . flip github req) =<< auth
