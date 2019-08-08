{-# LANGUAGE DerivingVia #-}
module Futurice.App.Reports.ValueCreation where

import Futurice.Integrations
import Futurice.Generics
import Futurice.Prelude
import Prelude ()
import Futurice.Time.Month (monthYear)

import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ

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

valueCreationReport :: (MonadTime m, MonadPlanMillQuery m) => Maybe Integer -> m ValueCreationReport
valueCreationReport myear = do
    year <- maybe (monthYear <$> currentMonth) pure myear
    report <- PMQ.valueCreationByMonthReport year
    pure $ ValueCreationReport year $ fmap toRow $ toList $ PM.getValueCreationData report
  where
    toRow v = ValueCreationRow
        { valueRowTeam      = PM._pvcTeam v
        , valueRowPerson    = PM._pvcPerson v
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
