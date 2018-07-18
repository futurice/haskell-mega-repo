{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.ProxyMgmt.Utils where

import Futurice.Postgres
import Futurice.Prelude
import Futurice.Servant  (cachedIO)
import Prelude ()
import System.Entropy    (getEntropy)

import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set

import Futurice.App.Proxy.API
import Futurice.App.ProxyMgmt.Ctx
import Futurice.App.ProxyMgmt.Types

-------------------------------------------------------------------------------
-- Generate token
-------------------------------------------------------------------------------

generateToken :: IO Text
generateToken = do
    bytes <- getEntropy 30
    let base64 =  Base64.encode bytes
        base64T = decodeUtf8Lenient base64
    return base64T

-------------------------------------------------------------------------------
-- Fetch data
-------------------------------------------------------------------------------

fetchPolicies :: Ctx f -> IO [PolicyName]
fetchPolicies Ctx {..} =
    runLogT "fetchPolicies" ctxLogger $ do
        fromOnly <$$> safePoolQuery_ ctxPostgresPool
            "SELECT policyname FROM proxyapp.policy ORDER BY policyname;"

fetchPolicyEndpoints :: Ctx f -> IO (Map PolicyName (Set LenientEndpoint))
fetchPolicyEndpoints Ctx {..} = runLogT "fetchPolicies" ctxLogger $ do
    -- we need to ask for policies to get empty policies
    policies <- fromOnly <$$> safePoolQuery_ ctxPostgresPool
        "SELECT policyname FROM proxyapp.policy ORDER BY policyname;"
    endpoints <- safePoolQuery_ ctxPostgresPool
        "SELECT policyname, endpoint FROM proxyapp.policy_endpoint;"
    return (mk policies endpoints)
  where
    mk policies endpoints = Map.fromListWith (<>) $
        [ (policy, mempty) | policy <- policies ] ++
        [ (policy, Set.singleton endpoint) | (policy, endpoint) <- endpoints ]

fetchTokens :: Ctx f -> IO [Token]
fetchTokens Ctx {..} =
    runLogT "fetchTokens" ctxLogger $ do
        safePoolQuery_ ctxPostgresPool
            "SELECT username, passtext is not null, usertype, policyname FROM proxyapp.credentials ORDER BY username ASC;"

fetchAccessEntries :: Ctx f -> IO [AccessEntry]
fetchAccessEntries Ctx {..} =
    cachedIO ctxLogger ctxCache 600 () $ runLogT "fetchAccessEntries" ctxLogger $ do
        safePoolQuery_ ctxPostgresPool
            "SELECT username, updated, endpoint FROM proxyapp.accesslog WHERE current_timestamp - updated < '6 months' :: interval ORDER BY updated DESC;"


