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

import qualified Data.Set         as Set
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
    , prjOperationalId :: !(Maybe Int)

    -- these fields are asked for dashboards:
    , prjStart                      :: !(Maybe UTCTime)
    , prjFinish                     :: !(Maybe UTCTime)
    , prjProjectManager             :: !(Maybe PM.UserId)

    -- better types would be... better
    , prjInvoicedRevenue            :: !Double
    , prjActualRevenue              :: !Double
    , prjTotalRevenue               :: !Double
    , prjActualCost                 :: !Double
    , prjTotalCost                  :: !Double
    , prjActualEffort               :: !Int
    , prjTotalEffort                :: !Int
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
    => [PM.AccountId]
    -> m [Project]
projectsData fAccIds = do
    prjs <- PMQ.projects
    cats <- PMQ.allEnumerationValues Proxy Proxy
    return $ sortOn prjName $ mapMaybe (convert cats) (toList prjs)
  where
    convert cats p = do
        accId <- p ^. PM.pAccount
        guard (predAccount accId)
        return Project
            { prjProjectId = p ^. PM.identifier
            , prjAccountId = accId
            , prjName      = p ^. PM.pName
            , prjCategory  = fromMaybe "-" $ cats ^? ix (p ^. PM.pCategory)
            , prjOperationalId = p ^. PM.pOperationalId

            , prjStart           = PM.pStart p
            , prjFinish          = PM.pFinish p
            , prjProjectManager  = PM.pProjectManager p
            , prjInvoicedRevenue = PM.pInvoicedRevenue p
            , prjActualRevenue   = PM.pActualRevenue p
            , prjTotalRevenue    = PM.pTotalRevenue p
            , prjActualCost      = PM.pActualCost p
            , prjTotalCost       = PM.pTotalCost p
            , prjActualEffort    = PM.pActualEffort p
            , prjTotalEffort     = PM.pTotalEffort p
            }

    predAccount accId = case fAccIds of
        [] -> True
        xs -> accId `elem` Set.fromList xs
