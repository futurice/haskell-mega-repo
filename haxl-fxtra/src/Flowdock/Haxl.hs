{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Flowdock.Haxl (
    organisation,
    messagesSince,
    initDataSource,
    initDataSource',
    FlowdockRequest(..),
    ) where

import Futurice.Prelude
import Haxl.Core

import qualified Chat.Flowdock.REST    as FD
import qualified Chat.Flowdock.REST.IO as FDIO

data FlowdockRequest a where
    FetchOrganisation
        :: FD.ParamName FD.Organisation
        -> FlowdockRequest FD.Organisation
    FetchMessages
        :: FD.ParamName FD.Organisation
        -> FD.ParamName FD.Flow
        -> Maybe FD.MessageId
        -> FlowdockRequest [FD.Message]

deriving instance Show (FlowdockRequest a)
deriving instance Typeable FlowdockRequest
deriving instance Eq (FlowdockRequest a)

instance Haxl.Core.ShowP FlowdockRequest where showp = show

instance Hashable (FlowdockRequest a) where
  hashWithSalt salt (FetchOrganisation org) = salt `hashWithSalt` (0::Int)
       `hashWithSalt` org
  hashWithSalt salt (FetchMessages org flow since) = salt `hashWithSalt` (1::Int)
      `hashWithSalt` org
      `hashWithSalt` flow
      `hashWithSalt` since

organisation :: FD.ParamName FD.Organisation -> GenHaxl u FD.Organisation
organisation = dataFetch . FetchOrganisation

messagesSince
    :: FD.ParamName FD.Organisation
    -> FD.ParamName FD.Flow
    -> Maybe FD.MessageId
    -> GenHaxl u [FD.Message]
messagesSince org flow since = dataFetch (FetchMessages org flow since)

instance StateKey FlowdockRequest where
    data State FlowdockRequest = FlowdockDataState Manager FD.AuthToken

initDataSource
    :: FD.AuthToken           -- ^ Authentication token
    -> IO (State FlowdockRequest)
initDataSource auth = do
    mgr <- newManager tlsManagerSettings
    pure (FlowdockDataState mgr auth)

initDataSource'
    :: Manager
    -> FD.AuthToken           -- ^ Authentication token
    -> State FlowdockRequest
initDataSource' = FlowdockDataState

instance DataSourceName FlowdockRequest where
  dataSourceName _ = "FlowdockDataSource"

instance DataSource u FlowdockRequest where
    -- TODO: hint immediatesubmit + background fetch
    fetch (FlowdockDataState mgr auth) _flags _userEnv = SyncFetch $ \blockedFetches ->
        batchFetch mgr auth blockedFetches

batchFetch :: Manager
           -> FD.AuthToken
           -> [BlockedFetch FlowdockRequest]
           -> IO ()
batchFetch mgr auth = mapM_ singleFetch
  where
    singleFetch :: BlockedFetch FlowdockRequest -> IO ()
    singleFetch (BlockedFetch (FetchOrganisation org) v) =
        action >>= putSuccess v
      where
        action = FDIO.organisation mgr auth org
    singleFetch (BlockedFetch (FetchMessages org flow since) v) =
        action >>= putSuccess v
      where
        action = FDIO.messages mgr auth org flow $ FD.defaultMessageOptions
            & FD.msgOptEvents  .~ [FD.EventMessage, FD.EventComment]
            & FD.msgOptLimit   ?~ 100
            & FD.msgOptSinceId .~ since
            & FD.msgOptSorting .~ FD.Ascending
