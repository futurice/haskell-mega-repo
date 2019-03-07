{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Google.Haxl where

import Control.Concurrent.ParallelIO.Local (parallel_, withPool)
import Futurice.Prelude
import Google
import Haxl.Core
import Prelude ()

newtype GoogleRequest a = GR (Req a) deriving (Show,Eq)

instance ShowP GoogleRequest where showp = show

instance Hashable (GoogleRequest a) where
    hashWithSalt salt (GR req) = hashWithSalt salt req

instance StateKey GoogleRequest where
    data State GoogleRequest = GoogleState GoogleCredentials Manager

initDataSource
    :: GoogleCredentials -- ^ Credentials to Google API
    -> Manager           -- ^ HTTP Manager
    -> State GoogleRequest
initDataSource = GoogleState

instance DataSourceName GoogleRequest where
    dataSourceName _ = "GoogleDataSource"

instance DataSource u GoogleRequest where
    fetch = googleFetch

googleFetch :: State GoogleRequest -> Flags -> u -> PerformFetch GoogleRequest
googleFetch (GoogleState cred mgr) _f _u = SyncFetch $ batchFetch cred mgr

batchFetch :: GoogleCredentials -> Manager -> [BlockedFetch GoogleRequest] -> IO ()
batchFetch cred mgr fetches =
    withPool 10 $ \pool ->
        parallel_ pool (doFetch cred mgr <$> fetches)

doFetch :: GoogleCredentials -> Manager -> BlockedFetch GoogleRequest -> IO ()
doFetch cred mgr (BlockedFetch (GR r) v) = do
    res <- evalGoogleReqIO cred mgr r
    putSuccess v res

events :: ReadOnlyScope -> Day -> Day -> Text -> GenHaxl u [Event]
events readonly x y z = dataFetch $ GR $ ReqEvents readonly x y z

calendarResources :: ReadOnlyScope -> GenHaxl u [CalendarResource]
calendarResources = dataFetch . GR . ReqCalendarResources
