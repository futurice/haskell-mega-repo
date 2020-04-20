{-# LANGUAGE DerivingVia #-}
module Futurice.App.Reports.TeamsHoursByCategoryReport where

import Data.Fixed            (Centi)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Prelude
import Futurice.Time.Month   (monthInterval)
import Prelude ()

import qualified Data.Vector      as V
import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ

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
    } deriving (ToSchema, GhcGeneric, SopGeneric, HasDatatypeInfo)
      deriving (FromJSON, ToJSON) via (Sopica TeamsHoursByCategoryRow)

data TeamsHoursByCategoryReport = TeamsHoursByCategoryReport
    { thrStartDay :: !Day
    , thrEndDay   :: !Day
    , thrData     :: ![TeamsHoursByCategoryRow]
    } deriving (ToSchema, GhcGeneric, SopGeneric, HasDatatypeInfo)
      deriving (FromJSON, ToJSON) via (Sopica TeamsHoursByCategoryReport)

teamsHoursByCategoryReport :: (MonadPlanMillQuery m) => Month -> m TeamsHoursByCategoryReport
teamsHoursByCategoryReport month = do
    let interval = monthInterval month
    report <- PMQ.teamsHoursByCategoryReport $ monthInterval month
    pure $ TeamsHoursByCategoryReport
        { thrStartDay = minimum interval
        , thrEndDay   = maximum interval
        , thrData     = rowToRow <$> (V.toList report)
        }
  where
    rowToRow r = TeamsHoursByCategoryRow
        { _thcName              = PM._thcName r
        , _thcCustomerWork      = PM._thcCustomerWork r
        , _thcSales             = PM._thcSales r
        , _thcFuturiceInternal  = PM._thcFuturiceInternal r
        , _thcTeamInternalWork  = PM._thcTeamInternalWork r
        , _thcAbsences          = PM._thcAbsences r
        , _thcUTZ               = PM._thcUTZ r
        , _thcValueCreation     = PM._thcValueCreation r
        , _thcTotal             = PM._thcTotal r
        , _thcPrimaryTeam       = PM._thcPrimaryTeam r
        , _thcPrimaryCompetence = PM._thcPrimaryCompetence r
        }
