{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
module Personio.Haxl (
    request,
    initDataSource,
    PersonioRequest(..),
    PersonioBatchError(..),
    ) where

import Futurice.Prelude
import Haxl.Core

import qualified Data.Aeson.Compat as Aeson
import qualified Personio
import qualified Network.HTTP.Client    as HTTP

newtype PersonioRequest a = PR (Personio.PersonioReq a)
  deriving (Eq, Show)

instance Hashable (PersonioRequest a) where
    hashWithSalt salt (PR req) = hashWithSalt salt req

instance Haxl.Core.ShowP PersonioRequest where showp = show

request :: (Show a, Typeable a) => Personio.PersonioReq a -> GenHaxl u a
request = dataFetch . PR

instance StateKey PersonioRequest where
    data State PersonioRequest = PersonioState Logger Manager HTTP.Request

initDataSource
    :: Logger                -- ^ Logger
    -> Manager               -- ^ HTTP manager
    -> HTTP.Request          -- ^ Base request
    -> State PersonioRequest
initDataSource = PersonioState

instance DataSourceName PersonioRequest where
    dataSourceName _ = "PersonioDataSource"

instance DataSource u PersonioRequest where
    fetch (PersonioState _lgr mgr baseReq) _f _u = SyncFetch $ \blockedFetches -> do
        res <- HTTP.httpLbs req mgr
        Personio.SomePersonioRes pReq pRes <- Aeson.decode (HTTP.responseBody res)
        case pReq of
            Personio.PersonioAll -> uncurry complete pRes blockedFetches 
            _                    -> failure blockedFetches
      where
        req = baseReq
           { HTTP.requestHeaders
                = ("Content-Type", "application/json")
                : ("Accept", "application/json")
                : HTTP.requestHeaders baseReq
            , HTTP.method
                = "POST"
            , HTTP.requestBody
                = HTTP.RequestBodyLBS $ Aeson.encode $
                    Personio.SomePersonioReq Personio.PersonioAll
            }

        failure :: [BlockedFetch PersonioRequest] -> IO ()
        failure = traverse_ $ \(BlockedFetch _ v) ->
            putFailure v (PersonioBatchError "inconsistent response from proxy")

        complete
            :: [Personio.Employee]
            -> [Personio.EmployeeValidation]
            -> [BlockedFetch PersonioRequest]
            -> IO ()
        complete es vs = traverse_ $ \(BlockedFetch (PR r) v) -> k v r
          where
            k :: ResultVar a -> Personio.PersonioReq a -> IO ()
            k v Personio.PersonioAll         = putSuccess v (es, vs)
            k v Personio.PersonioEmployees   = putSuccess v es
            k v Personio.PersonioValidations = putSuccess v vs


-- | Personio batch error
newtype PersonioBatchError = PersonioBatchError Text
    deriving (Show, Typeable)

instance Exception PersonioBatchError
