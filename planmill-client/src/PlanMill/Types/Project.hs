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
    PortfolioId (..),
    ProjectMember(..),
    ProjectMembers,
    Portfolios (..),
    Portfolio (..),
    ProjectType (..),
    ViewTemplate (..),
    viewTemplateToInt,
    getPortfolio) where

import PlanMill.Internal.Prelude

import Futurice.Generics
import PlanMill.Types.Account                  (AccountId)
import PlanMill.Types.Enumeration              (EnumValue (..))
import PlanMill.Types.Identifier               (HasIdentifier (..), Identifier)
import PlanMill.Types.Meta                     (Meta (..))
import PlanMill.Types.User                     (UserId)
import Text.PrettyPrint.ANSI.Leijen.AnsiPretty (AnsiPretty (..))

import qualified Data.Aeson.Lens     as L
import qualified Data.Csv            as Csv
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified Options.SOP         as O

type ProjectId = Identifier Project
type Projects = Vector Project

newtype PortfolioId = PortfolioId Int --TODO: change to use Identifier
    deriving (Eq, Ord, Show, Read, Generic)
    deriving newtype ( AnsiPretty,Hashable, Binary, Structured, FromJSON, ToJSON, Csv.ToField, ToSchema, NFData)

newtype Portfolios = Portfolios (Vector Portfolio)
    deriving (Show, Generic)
    deriving newtype (AnsiPretty)

data ProjectType = CustomerProject | InternalProject deriving (Eq, Show, Generic)

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

checkProjectType :: Text -> Maybe ProjectType
checkProjectType s =
    let projectTypeString = T.toLower $ T.strip $ T.takeWhile (/= '\\') s
    in case projectTypeString of
      "customer projects" -> Just CustomerProject
      "internal projects" -> Just InternalProject
      _                   -> Nothing

toPortfolio :: (Text, Text) -> Maybe Portfolio
toPortfolio (k,v) =
    let getPortfolioName s = last $ T.splitOn "\\" s
    in Portfolio
       <$> (PortfolioId <$> readMaybe (T.unpack k))
       <*> checkProjectType v
       <*> (Just $ getPortfolioName v)

instance FromJSON Portfolios where
    parseJSON val =
        let filters = val ^? L.key "filters" . L._Array
            filters' = join $ listToMaybe . filter (\v -> v ^. L.key "name" . L._String == "portfolioFilter") . V.toList <$> filters
            portfolios = filters' ^. _Just . L.key "values" . L._Object
            res = mapMaybe toPortfolio $ map (\(k,v) -> (k, v ^. L._String)) $ HM.toList portfolios
        in pure $ Portfolios $ V.fromList res

getPortfolio :: Meta -> Portfolios
getPortfolio (Meta _ filters) =
    let portfolios = catMaybes . map toPortfolio . HM.toList <$> filters ^.at "portfolioFilter"
    in Portfolios $ V.fromList $ fromMaybe mempty portfolios

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
instance Structured Project

instance FromJSON Project where
    parseJSON = withObject "Project" $ \obj -> Project
        <$> obj .: "id"
        <*> (getParsedAsText <$> obj .: "name") -- HACK
        <*> obj .:? "account"
        <*> obj .:? "accountName"
        <*> (obj .:? "category" .!= EnumValue (-1)) -- seems not all projects have category?
        <*> (join <$> readMaybe <$$> obj .:? "operationalId")
        <*> obj .:? "portfolio"
        <*> obj .:? "start"
        <*> obj .:? "finish"
        <*> obj .:? "projectManager"
        <*> obj .:? "invoicedRevenue" .!= 0
        <*> obj .:? "actualRevenue"   .!= 0
        <*> obj .:? "totalRevenue"    .!= 0
        <*> obj .:? "actualCost"      .!= 0
        <*> obj .:? "totalCost"       .!= 0
        <*> obj .:? "actualEffort"    .!= 0
        <*> obj .:? "totalEffort"     .!= 0

data ProjectMember = ProjectMember
    { _projectMemberName   :: !Text
    , _projectMemberUserId :: !UserId
    } deriving (Eq, Ord, Show, Generic, Binary, NFData, Structured, Typeable)

instance FromJSON ProjectMember where
    parseJSON = withObject "ProjectMember" $ \p -> do
        ProjectMember <$> p .: "lastName"
                      <*> p .: "id"

type ProjectMembers = Vector ProjectMember
