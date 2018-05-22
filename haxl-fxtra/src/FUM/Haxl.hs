{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
module FUM.Haxl (
    request,
    fetchUsers,
    fetchList,
    initDataSource,
    initDataSource',
    FumRequest(..),
    ) where

import Futurice.Prelude

import Data.Aeson (FromJSON)
import Haxl.Core

import qualified FUM

data FumRequest a where
    FumRequest :: FromJSON a => FUM.FUM a -> FumRequest a

deriving instance Show (FumRequest a)
deriving instance Typeable FumRequest
deriving instance Eq (FumRequest a)

instance Haxl.Core.ShowP FumRequest where showp = show

instance Hashable (FumRequest a) where
    hashWithSalt salt (FumRequest req) =
        salt `hashWithSalt` req

request
    :: (Show a, Typeable a, FromJSON a)
    => FUM.FUM a -> GenHaxl u a
request = dataFetch . FumRequest

fetchUsers :: GenHaxl u (Vector FUM.User)
fetchUsers = request FUM.fumUsersR

fetchList :: FUM.ListName -> GenHaxl u (Vector FUM.User)
fetchList = request . FUM.fumListR

instance StateKey FumRequest where
    data State FumRequest = FumState Manager FUM.AuthToken FUM.BaseUrl

-- | /TODO:/ remove in favour of 'initDataSource''
initDataSource
    :: FUM.AuthToken          -- ^ Authentication token
    -> FUM.BaseUrl            -- ^ Base url
    -> IO (State FumRequest)
initDataSource token baseUrl = do
    mgr <- newManager tlsManagerSettings
    pure (initDataSource' mgr token baseUrl)

initDataSource'
    :: Manager                -- ^ HTTP manager
    -> FUM.AuthToken          -- ^ Authentication token
    -> FUM.BaseUrl            -- ^ Base url
    -> State FumRequest
initDataSource' = FumState

instance DataSourceName FumRequest where
    dataSourceName _ = "FumDataSource"

instance DataSource u FumRequest where
    -- TODO: hint dispatch immediately + background fetch
    fetch (FumState mgr token baseUrl) _flags _userEnv = SyncFetch $ \blockedFetches ->
        batchFetch mgr token baseUrl blockedFetches

batchFetch :: Manager
           -> FUM.AuthToken
           -> FUM.BaseUrl
           -> [BlockedFetch FumRequest]
           -> IO ()
batchFetch mgr token baseUrl = mapM_ (doFetch mgr token baseUrl)

doFetch :: Manager
        -> FUM.AuthToken
        -> FUM.BaseUrl
        -> BlockedFetch FumRequest
        -> IO ()
doFetch mgr token baseUrl (BlockedFetch (FumRequest req) v) =
    FUM.executeRequest mgr token baseUrl req >>= putSuccess v
