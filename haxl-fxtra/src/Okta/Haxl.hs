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
    data State OktaRequest = OktaState OktaCfg Logger Manager

instance DataSourceName OktaRequest where
    dataSourceName _ = "OktaDataSource"

instance DataSource u OktaRequest where
    fetch = oktaFetch

initDataSource
    :: OktaCfg -- ^ Credentials to Okta API
    -> Logger  -- ^ Logger
    -> Manager -- ^ HTTP Manager
    -> State OktaRequest
initDataSource = OktaState

doFetch :: OktaCfg -> Logger -> Manager -> BlockedFetch OktaRequest -> IO ()
doFetch cfg lgr mgr (BlockedFetch (OR r) v) = do
    res <- evalOktaReqIO cfg mgr lgr r
    putSuccess v res

batchFetch ::  OktaCfg -> Logger -> Manager -> [BlockedFetch OktaRequest] -> IO ()
batchFetch cfg lgr mgr fetches =
    withPool 10 $ \pool ->
        parallel_ pool (doFetch cfg lgr mgr <$> fetches)

oktaFetch :: State OktaRequest -> Flags -> u -> PerformFetch OktaRequest
oktaFetch (OktaState cfg lgr mgr) _f _u = SyncFetch $ batchFetch cfg lgr mgr

request :: (Show a, Typeable a) => Req a -> GenHaxl u w a
request = dataFetch . OR

fetchUsers :: GenHaxl u w [User]
fetchUsers = request ReqGetAllUsers
