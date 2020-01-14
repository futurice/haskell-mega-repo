{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Okta.Haxl where

import Control.Concurrent.ParallelIO.Local (parallel_, withPool)
import Futurice.Prelude
import Haxl.Core
import Okta
import Prelude ()

newtype OktaRequest a = OR (Req a) deriving (Show,Eq)

instance ShowP OktaRequest where showp = show

instance Hashable (OktaRequest a) where
    hashWithSalt salt (OR req) = hashWithSalt salt req

instance StateKey OktaRequest where
    data State OktaRequest = OktaState OktaCfg Manager

instance DataSourceName OktaRequest where
    dataSourceName _ = "OktaDataSource"

instance DataSource u OktaRequest where
    fetch = oktaFetch

initDataSource
    :: OktaCfg -- ^ Credentials to Okta API
    -> Manager -- ^ HTTP Manager
    -> State OktaRequest
initDataSource = OktaState

doFetch :: OktaCfg -> Manager -> BlockedFetch OktaRequest -> IO ()
doFetch cfg mgr (BlockedFetch (OR r) v) = do
    res <- evalOktaReqIO cfg mgr r
    putSuccess v res

batchFetch ::  OktaCfg -> Manager -> [BlockedFetch OktaRequest] -> IO ()
batchFetch cfg mgr fetches =
    withPool 10 $ \pool ->
        parallel_ pool (doFetch cfg mgr <$> fetches)

oktaFetch :: State OktaRequest -> Flags -> u -> PerformFetch OktaRequest
oktaFetch (OktaState cfg mgr) _f _u = SyncFetch $ batchFetch cfg mgr

request :: (Show a, Typeable a) => Req a -> GenHaxl u w a
request = dataFetch . OR

fetchUsers :: GenHaxl u w [User]
fetchUsers = request ReqGetAllUsers
