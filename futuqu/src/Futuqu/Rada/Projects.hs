{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futuqu.Rada.Projects where

import Futurice.Generics
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data Project = Project
    -- identifiers
    { prjProjectId :: !PM.ProjectId
    , prjAccountId :: !PM.AccountId
    -- planmill
    , prjName     :: !Text
    , prjCategory :: !Text -- TODO, make EnumTextValue
    -- TODO: add data as needed
    -- power
    -- TODO: power doesn't provide IDs so we could link data
    -- TODO: add data as needed
    }
  deriving stock (Eq, Ord, Show, GhcGeneric)
  deriving anyclass (NFData, SopGeneric, HasDatatypeInfo)

deriveVia [t| ToJSON Project         `Via` Sopica Project |]
deriveVia [t| FromJSON Project       `Via` Sopica Project |]
deriveVia [t| DefaultOrdered Project `Via` Sopica Project |]
deriveVia [t| ToRecord Project       `Via` Sopica Project |]
deriveVia [t| ToNamedRecord Project  `Via` Sopica Project |]

instance ToSchema Project where declareNamedSchema = sopDeclareNamedSchema

-------------------------------------------------------------------------------
-- Endpoint
-------------------------------------------------------------------------------

projectsData
    :: (MonadPlanMillQuery m, MonadMemoize m)
    => m [Project]
projectsData = do
    prjs <- PMQ.projects
    cats <- PMQ.allEnumerationValues Proxy Proxy
    return $ sortOn prjName $ mapMaybe (convert cats) (toList prjs)
  where
    convert cats p = do
        accId <- PM.pAccount p
        return Project
            { prjProjectId = p ^. PM.identifier
            , prjAccountId = accId
            , prjName      = PM.pName p
            , prjCategory  = fromMaybe "-" $ cats ^? ix (PM.pCategory p)
            }
