{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.Project (Project(..), Projects, ProjectId, ProjectMember(..), ProjectMembers) where

import PlanMill.Internal.Prelude

import PlanMill.Types.Account     (AccountId)
import PlanMill.Types.Enumeration (EnumValue (..))
import PlanMill.Types.Identifier  (HasIdentifier (..), Identifier)
import PlanMill.Types.User        (UserId)

type ProjectId = Identifier Project
type Projects = Vector Project

data Project = Project
    { _pId                        :: !ProjectId
    , pName                       :: !Text
    , pAccount                    :: !(Maybe AccountId)
    , pAccountName                :: !(Maybe Text)
    , pCategory                   :: !(EnumValue Project "category")

    -- these fields are asked for dashboards:
    , pStart                      :: !(Maybe UTCTime)
    , pFinish                     :: !(Maybe UTCTime)
    , pProjectManager             :: !(Maybe UserId)

    -- better types would be... better
    , pInvoicedRevenue            :: !Double
    , pActualRevenue              :: !Double
    , pTotalRevenue               :: !Double
    , pActualCost                 :: !Double
    , pTotalCost                  :: !Double
    , pActualEffort               :: !Int
    , pTotalEffort                :: !Int
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
    parseJSON = withObject "Project" $ \obj -> Project
        <$> obj .: "id"
        <*> (getParsedAsText <$> obj .: "name") -- HACK
        <*> obj .:? "account"
        <*> obj .:? "accountName"
        <*> (obj .:? "category" .!= EnumValue (-1)) -- seems not all projects have category?

        <*> (getU <$$> obj .:? "start")
        <*> (getU <$$> obj .:? "finish")
        <*> obj .:? "projectManager"

        -- it might be needed to change other fields to be lenient
        -- but let's see if we don't need to!
        <*> obj .:? "invoicedRevenue" .!= 0
        <*> obj .:? "actualRevenue"   .!= 0
        <*> obj .:? "totalRevenue"    .!= 0
        <*> obj .:? "actualCost"      .!= 0
        <*> obj .: "totalCost"
        <*> obj .:? "actualEffort"    .!= 0
        <*> obj .:? "totalEffort"     .!= 0

data ProjectMember = ProjectMember
    { _projectMemberName   :: !Text
    , _projectMemberUserId :: !UserId
    } deriving (Eq, Show, Generic, Binary, NFData, HasStructuralInfo, Typeable, HasSemanticVersion)

instance FromJSON ProjectMember where
    parseJSON = withObject "ProjectMember" $ \p -> do
        ProjectMember <$> p .: "lastName"
                      <*> p .: "id"

type ProjectMembers = Vector ProjectMember
