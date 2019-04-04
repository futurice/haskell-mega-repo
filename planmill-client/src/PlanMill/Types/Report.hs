{-# LANGUAGE OverloadedStrings #-}
module PlanMill.Types.Report (
    AllRevenues2,
    AllRevenuesPortfolio (..),
    Report,
    Reports,
    ReportsCategories,
    getRevenuesData) where

import Data.Aeson
import Data.Aeson.Types  (Parser)
import Data.Map          (fromListWith)
import Data.Vector
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import PlanMill.Internal.Prelude

data ReportsCategories = ReportsCategories
    { _reportsCategoriesProjectHomepage   :: !(Vector Reports)
    , _reportsCategoriesPortfolioHomepage :: !(Vector Reports)
    , _reportsCategoriesPortal            :: !(Vector Reports)
    } deriving (Generic)

instance AnsiPretty ReportsCategories

instance FromJSON ReportsCategories where
    parseJSON = withObject "Categories" $ \obj ->
      ReportsCategories <$> obj .: "Project homepage"
                        <*> obj .: "Portfolio homepage"
                        <*> obj .: "Portal"

data Reports = Reports
    { _reportsReports       :: !(Vector Report)
    , _reportsLocalizedName :: !Text
    } deriving (Generic)

instance AnsiPretty Reports

instance FromJSON Reports where
    parseJSON = withObject "Reports" $ \obj ->
      Reports <$> obj .: "reports"
              <*> obj .: "localizedName"

data Report = Report
    { _reportLocalizedName :: !Text
    , _reportModern        :: !Bool
    , _reportHasAccess     :: !Bool
    , _reportName          :: !Text
    , _reportFavorite      :: !Bool
    } deriving (Generic)

instance FromJSON Report where
    parseJSON = withObject "Report" $ \obj ->
      Report <$> obj .: "localizedName"
             <*> obj .: "modern"
             <*> obj .: "hasAccess"
             <*> obj .: "name"
             <*> obj .: "favorite"

instance AnsiPretty Report

newtype AllRevenues2 = AllRevenues2 {getRevenuesData :: Map Text [AllRevenuesPortfolio]}
    deriving (Eq, Show, Binary, Generic, ToSchema, ToJSON, NFData, HasSemanticVersion)

instance HasStructuralInfo AllRevenues2

instance AnsiPretty AllRevenues2

instance FromJSON AllRevenues2 where
    parseJSON v = do
        portfolios <- parseJSON v :: Parser (Vector AllRevenuesPortfolio)
        pure $ AllRevenues2 $ fromListWith (<>) $ fmap (\x -> (_arpPortfolioName x, [x])) $ Data.Vector.toList portfolios

data AllRevenuesPortfolio = AllRevenuesPortfolio
    { _arpPortfolioName     :: !Text
    , _arpCustomer          :: !Text
    , _arpProject           :: !Text
    , _arpProjectId         :: !(Maybe Int)
    , _arpValueCreation     :: !(Maybe Double) -- TODO: Think of better type for all these
    , _arpActualRevenue     :: !(Maybe Double)
    , _arpInvoicedNoVat     :: !(Maybe Double)
    , _arpActualHours       :: !(Maybe Double)
    , _arpActualNonBillable :: !(Maybe Double)
    , _arpSalesPrice        :: !(Maybe Double)
    , _arpEffectivePrice    :: !(Maybe Double)
    } deriving (Eq, Show, Binary, GhcGeneric, SopGeneric, ToSchema, HasDatatypeInfo, NFData)
      deriving (ToJSON) via (Sopica AllRevenuesPortfolio)

instance AnsiPretty AllRevenuesPortfolio
instance HasStructuralInfo AllRevenuesPortfolio where structuralInfo = sopStructuralInfo

instance FromJSON AllRevenuesPortfolio where
    parseJSON = withArray "Portfolio" $ \array -> do
        let parseNum :: (Read a) => Value -> Parser (Maybe a)
            parseNum s = do
                s' <- parseJSON s :: Parser (Maybe String)
                pure $ s' >>= readMaybe
        portfolioName <- traverse parseJSON (array !? 0)
        customer <- traverse parseJSON (array !? 1)
        project <- traverse parseJSON (array !? 2)
        projectId <- traverse parseNum (array !? 3)
        valueCreation <- traverse parseNum (array !? 4)
        actualRevenue <- traverse parseNum (array !? 5)
        invoicedNoVat <- traverse parseNum (array !? 6)
        actualHours <- traverse parseNum (array !? 7)
        actualNonBillable <- traverse parseNum (array !? 8)
        salesPrice <- traverse parseNum (array !? 9)
        effectivePrice <- traverse parseNum (array !? 10)
        let res = AllRevenuesPortfolio
                <$> portfolioName
                <*> customer
                <*> project
                <*> projectId
                <*> valueCreation
                <*> actualRevenue
                <*> invoicedNoVat
                <*> actualHours
                <*> actualNonBillable
                <*> salesPrice
                <*> effectivePrice
        case res of
          Just a -> pure a
          Nothing -> mempty
