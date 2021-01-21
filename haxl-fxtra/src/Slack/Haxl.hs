{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Slack.Haxl where

import Control.Concurrent.ParallelIO.Local (parallel_, withPool)
import Futurice.Prelude
import Haxl.Core
import Prelude ()
import Slack

newtype SlackRequest a = SL (Req a) deriving (Show,Eq)

instance ShowP SlackRequest where showp = show

instance Hashable (SlackRequest a) where
    hashWithSalt salt (SL req) = hashWithSalt salt req

instance StateKey SlackRequest where
    data State SlackRequest = SlackState SlackToken Logger Manager

instance DataSourceName SlackRequest where
    dataSourceName _ = "SlackDataSource"

instance DataSource u SlackRequest where
    fetch = slackFetch

initDataSource
    :: SlackToken -- ^ Token to Slack workspace
    -> Logger  -- ^ Logger
    -> Manager -- ^ HTTP Manager
    -> State SlackRequest
initDataSource = SlackState

doFetch :: SlackToken -> Logger -> Manager -> BlockedFetch SlackRequest -> IO ()
doFetch token _lgr mgr (BlockedFetch (SL r) v) = do
    res <- evalSlackReqIO token mgr r
    putSuccess v res

batchFetch ::  SlackToken -> Logger -> Manager -> [BlockedFetch SlackRequest] -> IO ()
batchFetch cfg lgr mgr fetches =
    withPool 10 $ \pool ->
        parallel_ pool (doFetch cfg lgr mgr <$> fetches)

slackFetch :: State SlackRequest -> Flags -> u -> PerformFetch SlackRequest
slackFetch (SlackState token lgr mgr) _f _u = SyncFetch $ batchFetch token lgr mgr

request :: (Show a, Typeable a) => Req a -> GenHaxl u w a
request = dataFetch . SL
