{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module PlanMill.Types.Report (
    AllRevenues2,
    AllRevenuesPortfolio (..),
    EarnedVacations,
    EarnedVacationsRow (..),
    Report,
    Reports,
    ReportsCategories,
    PersonValueCreations,
    PersonValueCreation (..),
    TeamsHoursByCategoryRow (..),
    TeamsHoursByCategory,
    getRevenuesData) where

import Data.Aeson
import Data.Aeson.Types  (Parser)
import Data.Fixed        (Centi)
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
    deriving (Eq, Show, Generic)
    deriving anyclass (Binary, ToSchema, NFData)
    deriving newtype (ToJSON)

instance Structured AllRevenues2

instance AnsiPretty AllRevenues2

instance FromJSON AllRevenues2 where
    parseJSON v = do
        portfolios <- parseJSON v :: Parser (Vector AllRevenuesPortfolio)
        pure $ AllRevenues2 $ fromListWith (<>) $ fmap (\x -> (_arpPortfolioName x, [x])) $ Data.Vector.toList portfolios

data AllRevenuesPortfolio = AllRevenuesPortfolio
    { _arpPortfolioName     :: !Text
    , _arpCustomer          :: !Text
    , _arpProject           :: !Text
    , _arpOperationalId     :: !(Maybe Int)
    , _arpValueCreation     :: !(Maybe Double) -- TODO: Think of better type for all these
    , _arpActualRevenue     :: !(Maybe Double)
    , _arpInvoicedNoVat     :: !(Maybe Double)
    , _arpActualHours       :: !(Maybe Double)
    , _arpActualNonBillable :: !(Maybe Double)
    , _arpSalesPrice        :: !(Maybe Double)
    , _arpEffectivePrice    :: !(Maybe Double)
    , _arpInvoiceNumber     :: !(Maybe Int)
    } deriving (Eq, Show, Binary, GhcGeneric, SopGeneric, ToSchema, HasDatatypeInfo, NFData)
      deriving (ToJSON) via (Sopica AllRevenuesPortfolio)

instance AnsiPretty AllRevenuesPortfolio
instance Structured AllRevenuesPortfolio

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
        invoiceNumber <- traverse parseNum (array !? 11)
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
                <*> invoiceNumber
        case res of
          Just a -> pure a
          Nothing -> mempty

data PersonValueCreation = PersonValueCreation
    { _pvcTeam           :: !Text
    , _pvcPerson         :: !Text
    , _pvcJanuaryValue   :: !Double --TODO: is the better type for this?
    , _pvcFebruaryValue  :: !Double
    , _pvcMarchValue     :: !Double
    , _pvcAprilValue     :: !Double
    , _pvcMayValue       :: !Double
    , _pvcJuneValue      :: !Double
    , _pvcJulyValue      :: !Double
    , _pvcAugustValue    :: !Double
    , _pvcSeptemberValue :: !Double
    , _pvcOctoberValue   :: !Double
    , _pvcNovemberValue  :: !Double
    , _pvcDecemberValue  :: !Double
    }  deriving (Eq, Show, Binary, GhcGeneric, SopGeneric, ToSchema, ToJSON, NFData, HasDatatypeInfo)
type PersonValueCreations = Vector PersonValueCreation

instance AnsiPretty PersonValueCreation
instance Structured PersonValueCreation

parseNum :: (Num a, Read a) => Value -> Parser a
parseNum s = do
    s' <- parseJSON s :: Parser String
    maybe mempty pure $ readMaybe s'

instance FromJSON PersonValueCreation where
    parseJSON = withArray "Valuecreation" $ \array -> do
        team <- traverse parseJSON (array !? 0)
        person <- traverse parseJSON (array !? 1)
        january <- traverse parseNum (array !? 2)
        february <- traverse parseNum (array !? 3)
        march <- traverse parseNum (array !? 4)
        april <- traverse parseNum (array !? 5)
        may <- traverse parseNum (array !? 6)
        june <- traverse parseNum (array !? 7)
        july <- traverse parseNum (array !? 8)
        august <- traverse parseNum (array !? 9)
        september <- traverse parseNum (array !? 10)
        october <- traverse parseNum (array !? 11)
        november <- traverse parseNum (array !? 12)
        december <- traverse parseNum (array !? 13)
        let res = PersonValueCreation
                <$> team
                <*> person
                <*> january
                <*> february
                <*> march
                <*> april
                <*> may
                <*> june
                <*> july
                <*> august
                <*> september
                <*> october
                <*> november
                <*> december
        case res of
          Just r -> pure r
          Nothing -> mempty

data TeamsHoursByCategoryRow = TeamsHoursByCategoryRow
    { _thcName              :: !Text
    , _thcCustomerWork      :: !Double
    , _thcSales             :: !Double
    , _thcFuturiceInternal  :: !Double
    , _thcTeamInternalWork  :: !Double
    , _thcAbsences          :: !Double
    , _thcUTZ               :: !Double
    , _thcValueCreation     :: !(Maybe Centi)
    , _thcTotal             :: !Double
    , _thcPrimaryTeam       :: !(Maybe Text)
    , _thcPrimaryCompetence :: !(Maybe Text)
    }  deriving (Eq, Show, Binary, GhcGeneric, SopGeneric, ToSchema, NFData, HasDatatypeInfo)

type TeamsHoursByCategory = Vector TeamsHoursByCategoryRow

instance AnsiPretty TeamsHoursByCategoryRow
instance Structured TeamsHoursByCategoryRow

instance FromJSON TeamsHoursByCategoryRow where
    parseJSON = withArray "Teams hours by category" $ \array -> do
        let parseNum :: Value -> Parser Double
            parseNum s = do
                s' <- parseJSON s :: Parser String
                case readMaybe s' of
                  Just s'' -> pure s''
                  Nothing -> mempty
        let parseFixed :: Value -> Parser (Maybe Centi)
            parseFixed s = do
                s' <- parseJSON s :: Parser (Maybe String)
                case s' >>= readMaybe of
                  Just s'' -> pure $ Just s''
                  Nothing -> pure Nothing
        name <- traverse parseJSON (array !? 0)
        customerWork <- traverse parseNum (array !? 1)
        sales <- traverse parseNum (array !? 2)
        futuriceInternal <- traverse parseNum (array !? 3)
        teamInternalWork <- traverse parseNum (array !? 4)
        absences <- traverse parseNum (array !? 5)
        utz <- traverse parseNum (array !? 6)
        valueCreation <- traverse parseFixed (array !? 7)
        total <- traverse parseNum (array !? 8)
        primaryTeam <- traverse parseJSON (array !? 9)
        primaryTeamCompetence <- traverse parseJSON (array !? 10)
        let res = TeamsHoursByCategoryRow
                <$> name
                <*> (fmap (/ 60) customerWork)
                <*> (fmap (/ 60) sales)
                <*> (fmap (/ 60) futuriceInternal)
                <*> (fmap (/ 60) teamInternalWork)
                <*> (fmap (/ 60) absences)
                <*> utz
                <*> valueCreation
                <*> (fmap (/ 60) total)
                <*> primaryTeam
                <*> primaryTeamCompetence
        maybe mempty pure res

type EarnedVacations = Vector EarnedVacationsRow

data EarnedVacationsRow = EarnedVacationsRow
    { _vacationUserName                :: !Text
    , _vacationUserHRNumber            :: !(Maybe Int)
    , _vacationCostCenter              :: !(Maybe Double)
    , _vacationOrganization            :: !Text
    , _vacationYear                    :: !(Maybe Int)
    , _vacationAnnualHoliday           :: !Double
    , _vacationUsedAnnualHoliday       :: !Double
    , _vacationAnnualHolidaysRemaining :: !Double
    , _vacationBonusHoliday            :: !Double
    , _vacationBonusHolidayRemaining   :: !Double
    , _vacationOtherAnnualHoliday      :: !Double
    , _vacationSavedLeave              :: !Double
    , _vacationSavedLeaveRemaining     :: !Double
    , _vacationSuperior                :: !(Maybe Double)
    } deriving (Eq, Show, Binary, NFData, GhcGeneric, SopGeneric, HasDatatypeInfo)

instance AnsiPretty EarnedVacationsRow
instance Structured EarnedVacationsRow

parseNull :: Value -> Parser (Maybe String)
parseNull Null = pure Nothing
parseNull val  = parseJSON val

instance FromJSON EarnedVacationsRow where
    parseJSON = withArray "earned-vacations-row" $ \arr -> do
       EarnedVacationsRow
           <$> parseJSON (arr ! 0)
           <*> (fmap (>>= readMaybe) (parseNull (arr ! 1)))
           <*> (fmap (>>= readMaybe) (parseNull (arr ! 2)))
           <*> parseJSON (arr ! 3)
           <*> (fmap (>>= readMaybe) (parseNull (arr ! 4)))
           <*> parseNum (arr ! 5)
           <*> parseNum (arr ! 6)
           <*> parseNum (arr ! 7)
           <*> parseNum (arr ! 8)
           <*> parseNum (arr ! 9)
           <*> parseNum (arr ! 10)
           <*> parseNum (arr ! 11)
           <*> parseNum (arr ! 12)
           <*> (fmap (>>= readMaybe) (parseNull (arr ! 13)))
