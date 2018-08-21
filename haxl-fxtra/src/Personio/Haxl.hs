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

import qualified Data.Aeson.Compat   as Aeson
import qualified Network.HTTP.Client as HTTP
import qualified Personio            as P

newtype PersonioRequest a = PR (P.PersonioReq a)
  deriving (Eq, Show)

instance Hashable (PersonioRequest a) where
    hashWithSalt salt (PR req) = hashWithSalt salt req

instance Haxl.Core.ShowP PersonioRequest where showp = show

request :: (Show a, Typeable a) => P.PersonioReq a -> GenHaxl u a
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
    fetch (PersonioState _lgr mgr baseReq) _f _u = SyncFetch $
        traverse_ single
      where
        single (BlockedFetch (PR r) v) = do
            res <- HTTP.httpLbs req mgr
            P.SomePersonioRes pReq pRes <- Aeson.decode (HTTP.responseBody res)
            -- TODO: PersonioReq should have geq instance
            case (r, pReq) of
                (P.PersonioAll,         P.PersonioAll)         -> putSuccess v pRes
                (P.PersonioEmployees,   P.PersonioEmployees)   -> putSuccess v pRes
                (P.PersonioActive,      P.PersonioActive)      -> putSuccess v pRes
                (P.PersonioValidations, P.PersonioValidations) -> putSuccess v pRes
                _ -> putFailure v $ PersonioBatchError "non matching req and res"
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
                        P.SomePersonioReq r
                }

-- | Personio batch error
newtype PersonioBatchError = PersonioBatchError Text
    deriving (Show, Typeable)

instance Exception PersonioBatchError
