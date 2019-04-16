{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}
module Futurice.App.Reports.PowerUTZ where

import Data.Fixed                           (Centi)
import Futuqu.Rada.Timereports
import Futurice.Generics
import Futurice.IdMap                       (key)
import Futurice.Integrations
import Futurice.Integrations.TimereportKind
import Futurice.Prelude
import Futurice.Time
import Prelude ()

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as Map
import qualified FUM
import qualified PlanMill            as PM

data PowerUTZReport = PowerUTZReport
    { powerUTZMonth   :: !Month
    , powerUTZPersons :: ![PowerUTZPerson]
    } deriving (GhcGeneric, SopGeneric, ToSchema, HasDatatypeInfo, NFData)
      deriving (ToJSON) via (Sopica PowerUTZReport)

data PowerUTZPerson = PowerUTZPerson
    { powerUTZUsername         :: !FUM.Login
    , powerUTZUtz              :: !Double
    , powerUTZBillableHours    :: !(NDT 'Hours Centi)
    , powerUTZNonBillableHours :: !(NDT 'Hours Centi)
    , powerUTZInternalHours    :: !(NDT 'Hours Centi)
    , powerUTZAbsenceHours     :: !(NDT 'Hours Centi)
--    , powerPMuid               :: !(PM.UserId)
    } deriving (GhcGeneric, SopGeneric, ToSchema, HasDatatypeInfo, NFData)
      deriving (ToJSON) via (Sopica PowerUTZPerson)

powerUTZReport :: (MonadPlanMillQuery m, MonadTime m, MonadPersonio m) => Maybe Month -> m PowerUTZReport
powerUTZReport mmonth = do
    month <- maybe currentMonth pure mmonth
    es <- personioPlanmillMap
    let es' = HM.toList es

    trs <- timereportsData month

    let trs' :: Map (PM.UserId, TimereportKind) (NDT 'Hours Centi)
        trs' = Map.filter (> 0) $ Map.fromListWith (+)
            [ ((trUserId tr, trKind tr), trHours tr)
            | tr <- trs
            ]

    return $
        PowerUTZReport month [ PowerUTZPerson
            { powerUTZUsername = fumLogin
            , powerUTZBillableHours    = billable
            , powerUTZNonBillableHours = nonBillable
            , powerUTZInternalHours    = internal
            , powerUTZAbsenceHours     = absence
            , powerUTZUtz       =
                  if total == 0
                  then 0
                  else 100 * realToFrac (unNDT (billable + nonBillable)) / realToFrac (unNDT total)
--            , powerPMuid = puser ^. key
            }

        | (fumLogin, (_, puser)) <- es'
        , let billable    = trs' ^. ix (puser ^. key, KindBillable)
        , let nonBillable = trs' ^. ix (puser ^. key, KindNonBillable)
        , let internal    = trs' ^. ix (puser ^. key, KindInternal)
        , let absence     = trs' ^. ix (puser ^. key, KindAbsence)
                          + trs' ^. ix (puser ^. key, KindSickLeave)

        , let total = billable + nonBillable + internal -- note: doesn't include absences.
        ]
