{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.Project (
    Project(..),
    Projects,
    ProjectId,
    ProjectMember(..),
    ProjectMembers,
    Portfolios,
    ViewTemplate (..),
    PortfolioId,
    viewTemplateToInt) where

import PlanMill.Internal.Prelude

import PlanMill.Types.Account                  (AccountId)
import PlanMill.Types.Enumeration              (EnumValue (..))
import PlanMill.Types.Identifier               (HasIdentifier (..), Identifier)
import PlanMill.Types.User                     (UserId)
import Text.PrettyPrint.ANSI.Leijen.AnsiPretty (AnsiPretty (..))

import qualified Data.Aeson.Lens     as L
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified Options.SOP         as O

type ProjectId = Identifier Project
type Projects = Vector Project

newtype PortfolioId = PortfolioId Int
    deriving (Eq, Ord, Show, Read, Generic)
    deriving newtype ( AnsiPretty,Hashable, Binary, HasStructuralInfo, FromJSON, NFData)

newtype Portfolios = Portfolios (Vector Portfolio)
    deriving (Generic)
    deriving newtype (AnsiPretty)

data ProjectType = CustomerProject | InternalProject deriving (Show, Generic)

instance AnsiPretty ProjectType where ansiPretty = ansiPretty . show

data ViewTemplate = AllExecutionProjects
                  | AllClosedProjects
                  deriving (Show,Read,Generic)

instance O.FromOptions ViewTemplate where
    optionsParser = O.argument O.auto (O.metavar ":projects-type")

viewTemplateToInt :: ViewTemplate -> Int
viewTemplateToInt AllExecutionProjects = 53
viewTemplateToInt AllClosedProjects = 55

data Portfolio = Portfolio
    { _portfolioId          :: !PortfolioId
    , _portfolioProjectType :: !ProjectType
    , _portfolioName        :: !Text
    } deriving (Show, Generic, AnsiPretty)

instance FromJSON Portfolios where
    parseJSON val =
        let filters = val ^? L.key "filters" . L._Array
            filters' = join $ listToMaybe . filter (\v -> v ^. L.key "name" . L._String == "portfolioFilter") . V.toList <$> filters
            portfolios = filters' ^. _Just . L.key "values" . L._Object
            checkProjectType s =
                let projectTypeString = T.toLower $ T.strip $ T.takeWhile (/= '\\') s
                in case projectTypeString of
                  "customer projects" -> Just CustomerProject
                  "internal projects" -> Just InternalProject
                  _                   -> Nothing
            getPortfolioName s = last $ T.splitOn "\\\\" s
            toPortfolio (k,v) = Portfolio
                <$> (PortfolioId <$> readMaybe (T.unpack k))
                <*> checkProjectType (v ^. L._String)
                <*> (Just $ getPortfolioName $ v ^. L._String)
            res = mapMaybe toPortfolio $ HM.toList portfolios
        in pure $ Portfolios $ V.fromList res

data Project = Project
    { _pId                        :: !ProjectId
    , _pName                      :: !Text
    , _pAccount                   :: !(Maybe AccountId)
    , pAccountName                :: !(Maybe Text)
    , _pCategory                  :: !(EnumValue Project "category")
    , _pOperationalId             :: !(Maybe Int)
    , _pPortfolioId               :: !(Maybe PortfolioId)

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
        <*> (join <$> readMaybe <$$> obj .:? "operationalId")
        <*> obj .:? "portfolio"
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
