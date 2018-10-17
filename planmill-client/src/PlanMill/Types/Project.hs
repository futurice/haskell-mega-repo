{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.Project (Project(..), Projects, ProjectId) where

import PlanMill.Internal.Prelude

import PlanMill.Types.Account     (AccountId)
import PlanMill.Types.Enumeration (EnumValue (..))
import PlanMill.Types.Identifier  (HasIdentifier (..), Identifier)
import PlanMill.Types.User        (UserId)

type ProjectId = Identifier Project
type Projects = Vector Project

-- [1]: present in projects/ID, [2] present in projects
data Project = Project
    { _pId                        :: !ProjectId
    , pName                       :: !Text
    , pAccount                    :: !(Maybe AccountId) -- @TODO AccountId
    , pAccountName                :: !(Maybe Text)
    , pActualCost                 :: !(Maybe Double) -- Currency unit
    , pActualEffort               :: !(Maybe Int)
    , pActualRevenue              :: !(Maybe Double) -- Currency unit
    , pBillableStatus             :: !(Maybe Int) -- @TODO enum;!SCHEMA NULL
    , pCategory                   :: !(EnumValue Project "category")
    , pFinish                     :: !(Maybe UTCTime)
    , pFixedRevenue               :: !(Maybe Double)
    -- , pFixedWork                  :: !(Maybe Double)
    , pFixedWorkEffort            :: !(Maybe Int)
    , pInvoiceAppendix            :: !(Maybe Int)
    , pInvoicedRevenue            :: !(Maybe Double) -- TODO: better type
    -- , pOperationalId              :: !(Maybe Int) -- TODO: failed to parse field operationalId: expected Int, encountered String"
    , pPlannedEffort              :: !(Maybe Int)
    , pPortfolio                  :: !Int  -- TODO: remove field (bustes cache)
    , pProjectManager             :: !(Maybe UserId)
    , pRemainingEffort            :: !(Maybe Int)
    , pReportedHours              :: !(Maybe Int)
    , pStart                      :: !(Maybe UTCTime)
    , pStatus                     :: !Int -- @TODO type
    , pTotalCost                  :: !(Maybe Double) -- TODO: better type
    , pTotalEffort                :: !(Maybe Int)
    , pTotalRemainingEffort       :: !(Maybe Int)
    , pTotalRevenue               :: !(Maybe Double) -- TODO: currency
    , pType                       :: !(Maybe Int) -- [1] @TODO type
    , pWorkCompleteness           :: !(Maybe Int) -- [2] !SCHEMA NULL
    , pWorkCompletenessPercentage :: !(Maybe Int) -- [2] !SCHEMA NULL
    }
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

makeLenses ''Project
deriveGeneric ''Project

instance HasKey Project where
    type Key Project = ProjectId
    key = pId

instance HasIdentifier Project Project where
    identifier = pId

instance Hashable Project
instance NFData Project
instance AnsiPretty Project
instance Binary Project
instance HasStructuralInfo Project where structuralInfo = sopStructuralInfo
instance HasSemanticVersion Project

instance FromJSON Project where
    parseJSON = withObject "Project" $ \obj ->
        Project <$> obj .: "id"
                <*> (getParsedAsText <$> obj .: "name") -- HACK
                <*> obj .:? "account"
                <*> obj .:? "accountName"
                <*> obj .:? "actualCost"
                <*> obj .:? "actualEffort"
                <*> obj .:? "actualRevenue"
                <*> obj .: "billableStatus"
                <*> (obj .:? "category" .!= EnumValue (-1)) -- seems not all projects have category?
                <*> (getU <$$> obj .:? "finish")
                <*> obj .: "fixedRevenue"
                -- <*> obj .: "fixedWork"
                <*> obj .: "fixedWorkEffort"
                <*> obj .:? "invoiceAppendix"
                <*> obj .:? "invoicedRevenue"
                -- <*> obj .: "operationalId"
                <*> obj .:? "plannedEffort"
                <*> (obj .:? "portfolio" .!= (-1)) -- TODO: remove field (bustes cache)
                <*> obj .:? "projectManager"
                <*> obj .:? "remainingEffort"
                <*> obj .:? "reportedHours"
                <*> (getU <$$> obj .:? "start")
                <*> obj .: "status"
                <*> obj .:? "totalCost"
                <*> obj .:? "totalEffort"
                <*> obj .:? "totalRemainingEffort"
                <*> obj .:? "totalRevenue"
                <*> obj .:? "type"
                <*> obj .:? "workCompleteness"
                <*> obj .:? "workCompletenessPercentage"
