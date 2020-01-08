{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Reports.ValueCreation where

import Futurice.Integrations
import Futurice.Generics
import Futurice.Prelude
import Prelude ()
import Futurice.Time.Month (monthYear)

import qualified Data.HashMap.Strict as HM
import qualified PlanMill         as PM
import qualified Personio           as P
import qualified PlanMill.Queries as PMQ
import qualified Data.Text           as T

-- Create a mapping for matching PlanMill names to PersonioID's
planmillNameToPersonioIdMap
    :: (MonadPersonio m)
    => m (HashMap (Text, Text) P.Employee)
planmillNameToPersonioIdMap = do
  us <- P.personio P.PersonioEmployees
  pure $ HM.fromList
       $ map (\p -> ((p ^. P.employeeFirst, p ^.  P.employeeLast), p))
       $ filter (\p -> p ^. P.employeeStatus == P.Active) us

data ValueCreationReport = ValueCreationReport
    { valueReportYear :: !Integer
    , valueData       :: ![ValueCreationRow]
    }
    deriving (GhcGeneric, SopGeneric)
    deriving anyclass (NFData, HasDatatypeInfo, ToSchema)
    deriving (FromJSON, ToJSON) via (Sopica ValueCreationReport)

data ValueCreationRow = ValueCreationRow
    { valueRowTeam      :: !Text
    , valueRowPerson    :: !Text
    , valueRowPersonioId  :: !(Maybe P.EmployeeId)
    , valueRowJanuary   :: !Double
    , valueRowFebruary  :: !Double
    , valueRowMarch     :: !Double
    , valueRowApril     :: !Double
    , valueRowMay       :: !Double
    , valueRowJune      :: !Double
    , valueRowJuly      :: !Double
    , valueRowAugust    :: !Double
    , valueRowSeptember :: !Double
    , valueRowOctober   :: !Double
    , valueRowNovember  :: !Double
    , valueRowDecember  :: !Double
    } deriving (GhcGeneric, SopGeneric, HasDatatypeInfo, NFData, ToSchema)
      deriving (FromJSON, ToJSON) via (Sopica ValueCreationRow)

valueCreationReport :: (MonadTime m, MonadPlanMillQuery m, MonadPersonio m) => Maybe Integer -> m ValueCreationReport
valueCreationReport myear = do
    year <- maybe (monthYear <$> currentMonth) pure myear
    report <- PMQ.valueCreationByMonthReport year
    nameMap <- planmillNameToPersonioIdMap
    pure $ ValueCreationReport year $ fmap (\x -> toRow nameMap x) $ toList $ PM.getValueCreationData report
  where
    nameSplit v = case (T.splitOn "," $ PM._pvcPerson v) of
                    (f:l:_) -> Just (f,l)
                    _       -> Nothing
    toRow nm v = ValueCreationRow
        { valueRowTeam      = PM._pvcTeam v
        , valueRowPerson    = PM._pvcPerson v
        , valueRowPersonioId = case nameSplit v >>= flip HM.lookup nm of
                                    Just p -> Just $ p ^. P.employeeId
                                    _      -> Nothing
        , valueRowJanuary   = PM._pvcJanuaryValue v
        , valueRowFebruary  = PM._pvcFebruaryValue v
        , valueRowMarch     = PM._pvcMarchValue v
        , valueRowApril     = PM._pvcAprilValue v
        , valueRowMay       = PM._pvcMayValue v
        , valueRowJune      = PM._pvcJuneValue v
        , valueRowJuly      = PM._pvcJulyValue v
        , valueRowAugust    = PM._pvcAugustValue v
        , valueRowSeptember = PM._pvcSeptemberValue v
        , valueRowOctober   = PM._pvcOctoberValue v
        , valueRowNovember  = PM._pvcNovemberValue v
        , valueRowDecember  = PM._pvcDecemberValue v
        }
