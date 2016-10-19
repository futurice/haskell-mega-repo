{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Futurice.App.Contacts.Executor (
    execute,
    ) where

import Futurice.Integrations hiding (Env)
import Futurice.Prelude

import Control.Monad.Reader      (ReaderT, runReaderT)
import Data.Aeson                (FromJSON)

import Haxl.Typed


-- Haxl data sources
import qualified Flowdock.TyHaxl as FDDataSource
import qualified FUM.TyHaxl      as FUMDataSource
import qualified Github.TyHaxl   as GHDataSource
--import qualified Haxl.Typed.HttpDataSource as HttpDataSource
--import qualified Haxl.Typed.LogDataSource  as LogDataSource

-- Integrations
import qualified Chat.Flowdock.REST as FD
import qualified FUM
import qualified GitHub             as GH

import Futurice.App.Contacts.Logic (ContactsM)

type Effects =
    '[ FUMDataSource.FumRequest
     , FDDataSource.FlowdockRequest
     , GHDataSource.GithubRequest
     ]

-------------------------------------------------------------------------------
-- Env for config
-------------------------------------------------------------------------------

data Env = Env
    { _envFumEmployeeListName      :: !FUM.ListName
    , _envFlowdockOrganisationName :: !(FD.ParamName FD.Organisation)
    , _envGithubOrganisationName   :: !(GH.Name GH.Organization )
    }

makeLenses ''Env

instance HasFUMEmployeeListName Env where
    fumEmployeeListName = envFumEmployeeListName
instance HasFlowdockOrgName Env where
    flowdockOrganisationName = envFlowdockOrganisationName
instance HasGithubOrgName Env where
    githubOrganisationName = envGithubOrganisationName

-------------------------------------------------------------------------------
-- execute
-------------------------------------------------------------------------------

-- | This is a monster gluing parts together.
execute
    :: forall a.
       (forall env n. ContactsM env n => n a)
    -> GH.Name GH.Organization       -- ^ Github organisation
    -> FD.ParamName FD.Organisation  -- ^ Flowdock organistion
    -> FUM.ListName                  -- ^ FUM user list
    -> FUM.AuthToken                 -- ^ FUM access token
    -> FUM.BaseUrl                   -- ^ FUM base url
    -> FD.AuthToken                  -- ^ Flowdock access token
    -> GH.Auth                       -- ^ Github access token
    -> IO a
execute action ghOrgName fdOrgName fumListName fumAuth fumBaseUrl fdAuth ghAuth = do
    fumDS  <- FUMDataSource.initDataSource fumAuth fumBaseUrl
    ghDS   <- GHDataSource.initDataSource ghAuth
    fdDS   <- FDDataSource.initDataSource fdAuth
    -- httpDS <- HttpDataSource.initDataSource
    -- logDS  <- LogDataSource.initDataSource
    environment <- initTyEnv (fumDS  `tyStateSet`
                              ghDS   `tyStateSet`
                              fdDS   `tyStateSet` tyStateEmpty) ()
                              -- httpDS `tyStateSet`
                              -- logDS  `tyStateSet` tyStateEmpty) ()
    runTyHaxl environment haxl
  where
    env  = Env fumListName fdOrgName ghOrgName
    haxl = toHaxl env action :: GenTyHaxl Effects () a

-------------------------------------------------------------------------------
-- Convert universal ContactsM action into conrete Haxl one
-------------------------------------------------------------------------------

toHaxl :: Env -> (forall n. ContactsM Env n => n a) -> GenTyHaxl Effects () a
toHaxl env action = runReaderT (unWrap action) env

newtype Wrap a = Wrap { unWrap :: ReaderT Env (GenTyHaxl Effects ()) a }
  deriving (Functor, Applicative, Monad, MonadReader Env)

instance MonadFlowdock Wrap where
    flowdockOrganisationReq = Wrap . lift . FDDataSource.organisation

instance MonadFUM Wrap where
    type MonadFUMC Wrap a = ShowTypeable a
    fumAction = Wrap . lift . FUMDataSource.request

instance MonadGitHub Wrap where
    type MonadGitHubC Wrap a = ShowTypeable a
    githubReq = Wrap . lift . GHDataSource.request

-------------------------------------------------------------------------------
-- Constraint for Wrap
-------------------------------------------------------------------------------

class (Eq a, Show a, Typeable a, FromJSON a) => ShowTypeable a
instance (Eq a, Show a, Typeable a, FromJSON a) => ShowTypeable (Vector a)
instance ShowTypeable GH.User
