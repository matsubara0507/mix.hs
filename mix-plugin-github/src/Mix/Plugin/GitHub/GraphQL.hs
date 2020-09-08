{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Mix.Plugin.GitHub.GraphQL where


import           RIO

import           Data.Aeson        (FromJSON)
import           Data.Extensible
import           Mix.Plugin.GitHub
import           Network.HTTP.Req

fetch
  :: (MonadIO m, MonadReader env m, HasGitHubToken env, FromJSON a)
  => Text -> m a
fetch query = do
  token <- asks (view tokenL)
  runReq defaultHttpConfig $ do
    r <- req POST url
      (ReqBodyJson $ #query @== query <: nil)
      jsonResponse
      (oAuth2Bearer token <> header "User-Agent" "mix.hs plugin github with req")
    pure $ responseBody r
  where
    url = https "api.github.com" /: "graphql"
