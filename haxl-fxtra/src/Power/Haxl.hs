{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Power.Haxl (
    request,
    initDataSource,
    PowerRequest(..),
    ) where

import Futurice.Prelude
import Haxl.Core

import qualified Network.HTTP.Client as HTTP
import qualified Power

newtype PowerRequest a = PR (Power.Req a)
  deriving (Eq, Show)

instance Hashable (PowerRequest a) where
    hashWithSalt salt (PR req) = hashWithSalt salt req

instance Haxl.Core.ShowP PowerRequest where showp = show

request :: Power.Req a -> GenHaxl u a
request Power.ReqPeople         = dataFetch (PR Power.ReqPeople)
request Power.ReqAllocation     = dataFetch (PR Power.ReqAllocation)
request Power.ReqCustomer       = dataFetch (PR Power.ReqCustomer)
request Power.ReqProject        = dataFetch (PR Power.ReqProject)
request Power.ReqProjectMapping = dataFetch (PR Power.ReqProjectMapping)

instance StateKey PowerRequest where
    data State PowerRequest = PowerState Logger Manager HTTP.Request

initDataSource
    :: Logger                -- ^ Logger
    -> Manager               -- ^ HTTP manager
    -> HTTP.Request          -- ^ Base request
    -> State PowerRequest
initDataSource = PowerState

instance DataSourceName PowerRequest where
    dataSourceName _ = "PowerDataSource"

instance DataSource u PowerRequest where
    fetch (PowerState _lgr mgr req) _f _u = SyncFetch $
        traverse_ single
      where
        single (BlockedFetch (PR r) v) = do
            res <- Power.evalIOReq req mgr r
            case res of
                Left exc -> putFailure v exc
                Right x ->  putSuccess v x
