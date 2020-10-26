{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Github.Haxl (
    request,
    membersOf,
    userInfoFor,
    initDataSource,
    GithubRequest(..),
    GithubDataSourceException(..),
    ) where

import Futurice.Generics
import Futurice.Prelude

import Control.Concurrent.ParallelIO.Local (parallel_, withPool)

import Control.Exception
import Haxl.Core

import qualified GitHub as GH

data GithubRequest a where
    GithubRequest :: (FromJSON a, Show a) => GH.Request 'GH.RA a -> GithubRequest a

deriving instance Show (GithubRequest a)
deriving instance Typeable GithubRequest
deriving instance Eq a => Eq (GithubRequest a)

instance Haxl.Core.ShowP GithubRequest where showp = show

instance Hashable (GithubRequest a) where
  hashWithSalt salt (GithubRequest gh) = hashWithSalt salt gh

request :: (Eq a, Show a, Typeable a, FromJSON a) => GH.Request 'GH.RA a -> GenHaxl u w a
request = dataFetch . GithubRequest

membersOf :: GH.Name GH.Organization -> GenHaxl u w (Vector GH.SimpleUser)
membersOf = request . flip GH.membersOfR GH.FetchAll

userInfoFor :: GH.Name GH.User -> GenHaxl u w GH.User
userInfoFor = request . GH.userInfoForR

instance StateKey GithubRequest where
    data State GithubRequest = GithubDataState GH.Auth

initDataSource
    :: GH.Auth           -- ^ Authentication token
    -> IO (State GithubRequest)
initDataSource auth =
    pure (GithubDataState auth)

instance DataSourceName GithubRequest where
    dataSourceName _ = "GithubDataSource"

instance DataSource u GithubRequest where
    fetch (GithubDataState auth) _flags _userEnv = SyncFetch $ \blockedFetches ->
        batchFetch auth blockedFetches

batchFetch :: GH.Auth  -> [BlockedFetch GithubRequest] -> IO ()
batchFetch auth fetches =
    withPool 10 $ \pool ->
        parallel_ pool (doFetch auth <$> fetches)

putEither :: ResultVar a -> Either String a -> IO ()
putEither v res = case res of
    Right x -> putSuccess v x
    Left err -> putFailure v (GithubDataSourceException err)

doFetch :: GH.Auth -> BlockedFetch GithubRequest -> IO ()
doFetch auth (BlockedFetch (GithubRequest req) v) =
    action >>= putEither v
  where
    action = first show <$> GH.executeRequest auth req

newtype GithubDataSourceException = GithubDataSourceException String
  deriving (Show, Typeable)

instance Exception GithubDataSourceException where
    toException = transientErrorToException
    fromException = transientErrorFromException
