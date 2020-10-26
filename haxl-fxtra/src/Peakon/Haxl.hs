{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Peakon.Haxl where

import Control.Concurrent.ParallelIO.Local (parallel_, withPool)
import Futurice.Prelude
import Haxl.Core
import Peakon
import Prelude ()

newtype PeakonRequest a = PK (Req a) deriving (Show,Eq)

instance ShowP PeakonRequest where showp = show

instance Hashable (PeakonRequest a) where
    hashWithSalt salt (PK req) = hashWithSalt salt req

instance StateKey PeakonRequest where
    data State PeakonRequest = PeakonState PeakonCfg Logger Manager

instance DataSourceName PeakonRequest where
    dataSourceName _ = "PeakonDataSource"

instance DataSource u PeakonRequest where
    fetch = peakonFetch

initDataSource
    :: PeakonCfg -- ^ Credentials to Peakon API
    -> Logger  -- ^ Logger
    -> Manager -- ^ HTTP Manager
    -> State PeakonRequest
initDataSource = PeakonState

doFetch :: PeakonCfg -> Logger -> Manager -> BlockedFetch PeakonRequest -> IO ()
doFetch cfg lgr mgr (BlockedFetch (PK r) v) = do
    res <- evalPeakonReqIO cfg mgr lgr r
    putSuccess v res

batchFetch ::  PeakonCfg -> Logger -> Manager -> [BlockedFetch PeakonRequest] -> IO ()
batchFetch cfg lgr mgr fetches =
    withPool 10 $ \pool ->
        parallel_ pool (doFetch cfg lgr mgr <$> fetches)

peakonFetch :: State PeakonRequest -> Flags -> u -> PerformFetch PeakonRequest
peakonFetch (PeakonState cfg lgr mgr) _f _u = SyncFetch $ batchFetch cfg lgr mgr

request :: (Show a, Typeable a) => Req a -> GenHaxl u w a
request = dataFetch . PK
