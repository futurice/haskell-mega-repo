{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- | Missing hours report
module Futurice.App.Reports.PowerProjects (
    -- * Report
    PowerProjectsReport,
    powerProjectsReport,
    -- * Types
    PowerAccount (..),
    PowerProject (..),
    ) where

import Data.Set.Lens           (setOf)
import Data.Vector.Lens        (vector)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Prelude
import Futurice.Report.Columns
import Prelude ()

import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data PowerProject = PowerProject
    { _powerProjectId        :: !PM.ProjectId
    , _powerProjectAccountId :: !(Maybe PM.AccountId)
    , _powerProjectName      :: !Text
    }
  deriving (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (NFData)

deriveGeneric ''PowerProject
deriveVia [t| ToJSON PowerProject   `Via` Sopica PowerProject |]
deriveVia [t| FromJSON PowerProject `Via` Sopica PowerProject |]
instance ToColumns PowerProject
instance ToSchema PowerProject where declareNamedSchema = sopDeclareNamedSchema

data PowerAccount = PowerAccount
    { _powerAccountId        :: !PM.AccountId
    , _powerAccountName      :: !Text
    }
  deriving (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (NFData)

deriveGeneric ''PowerAccount
deriveVia [t| ToJSON PowerAccount   `Via` Sopica PowerAccount |]
deriveVia [t| FromJSON PowerAccount `Via` Sopica PowerAccount |]
instance ToColumns PowerAccount
instance ToSchema PowerAccount where declareNamedSchema = sopDeclareNamedSchema


-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

data PowerProjectsReport = PowerProjectsReport
    { powerProjects :: !(Vector PowerProject)
    , powerAccounts :: !(Vector PowerAccount)
    }
  deriving (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (NFData)

deriveGeneric ''PowerProjectsReport
deriveVia [t| ToJSON PowerProjectsReport   `Via` Sopica PowerProjectsReport |]
deriveVia [t| FromJSON PowerProjectsReport `Via` Sopica PowerProjectsReport |]
instance ToColumns PowerProjectsReport
instance ToSchema PowerProjectsReport where declareNamedSchema = sopDeclareNamedSchema

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

powerProjectsReport
    :: forall m.
        ( MonadPlanMillQuery m
        )
    => m PowerProjectsReport
powerProjectsReport = do
    ps <- PMQ.projects
    -- we don't have query for all accounts, but it's almost as efficient
    -- to fetch based on aids list.
    --
    -- "Benefit": we will have only the accounts with projects
    --
    let aids = toList (setOf (folded . PM.pAccount . folded) ps)
    as <- traverse PMQ.account aids
    pure PowerProjectsReport
        { powerProjects = toPower <$> ps
        , powerAccounts = toPowerA <$> as ^. vector
        }
  where
    toPower :: PM.Project -> PowerProject
    toPower p = PowerProject
        { _powerProjectId        = p ^. PM.identifier
        , _powerProjectAccountId = PM._pAccount p
        , _powerProjectName      = PM._pName p
        }

    toPowerA :: PM.Account -> PowerAccount
    toPowerA a = PowerAccount
        { _powerAccountId = a ^. PM.identifier
        , _powerAccountName = PM.saName a
        }
