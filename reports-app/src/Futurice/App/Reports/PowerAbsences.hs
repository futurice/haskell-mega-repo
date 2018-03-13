{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- | Missing hours report
module Futurice.App.Reports.PowerAbsences (
    -- * Report
    PowerAbsenceReport,
    powerAbsenceReport,
    -- * Types
    PowerAbsence (..),
    -- * Lenses
    powerAbsenceUsername,
    powerAbsenceStart,
    powerAbsenceEnd,
    powerAbsencePlanmillId,
    powerAbsenceCapacities,
    powerAbsenceBusinessDays,
    ) where

import Data.Fixed                (Centi)
import Data.Time                 (diffDays)
import Data.Tuple                (swap)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Prelude
import Futurice.Report.Columns
import Futurice.Time
import Numeric.Interval.NonEmpty ((...))
import Prelude ()

import qualified Data.HashMap.Strict       as HM
import qualified Data.Map.Strict           as Map
import qualified Data.Vector               as V
import qualified FUM
import qualified Numeric.Interval.NonEmpty as I
import qualified PlanMill                  as PM
import qualified PlanMill.Queries          as PMQ

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data PowerAbsence = PowerAbsence
    { _powerAbsenceUsername     :: !(Maybe FUM.Login)
    , _powerAbsenceStart        :: !Day
    , _powerAbsenceEnd          :: !Day
    , _powerAbsencePlanmillId   :: !PM.AbsenceId
    , _powerAbsenceCapacities   :: !(Map Day :$ NDT 'Hours Centi)
    , _powerAbsenceBusinessDays :: !(NDT 'Days Int)
    }
  deriving (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (NFData)

makeLenses ''PowerAbsence
deriveGeneric ''PowerAbsence
deriveVia [t| ToJSON PowerAbsence   `Via` Sopica PowerAbsence |]
deriveVia [t| FromJSON PowerAbsence `Via` Sopica PowerAbsence |]
instance ToSchema PowerAbsence where declareNamedSchema = sopDeclareNamedSchema

instance ToColumns PowerAbsence where
    type Columns PowerAbsence =
        '[ Maybe FUM.Login, Day, Day, NDT 'Days Int, NDT 'Days Int ]

    columnNames _ =
        K "fum" :*
        K "start" :*
        K "end" :*
        K "calendar-days" :*
        K "business-days" :*
        Nil

    toColumns ab  = pure $
        I (ab ^. powerAbsenceUsername) :*
        I (ab ^. powerAbsenceStart) :*
        I (ab ^. powerAbsenceEnd) :*
        I (NDT $ fromInteger $ 1 +
            diffDays (ab ^. powerAbsenceEnd) (ab ^. powerAbsenceStart)) :*
        I (ab ^. powerAbsenceBusinessDays) :*
        Nil

-- instance DefaultOrdered PowerAbsence where headerOrder = sopHeaderOrder
-- instance ToNamedRecord PowerAbsence where toNamedRecord = sopToNamedRecord

-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

type PowerAbsenceReport = Vector PowerAbsence

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

powerAbsenceReport
    :: forall m env.
        ( PM.MonadTime m, MonadPersonio m, MonadPlanMillQuery m
        , MonadReader env m, HasFUMEmployeeListName env
        )
    => Maybe Month
    -> m PowerAbsenceReport
powerAbsenceReport mmonth = do
    month <- maybe currentMonth pure mmonth
    let startDay = firstDayOfMonth month
    let endDay   = lastDayOfMonth month
    let interval = startDay ... endDay
    -- Users
    fpm <- snd <$$> personioPlanmillMap
    -- Fetch all absences, on purpose
    as0 <- PMQ.absences
    -- Take intervals which overlap our interval of the interest
    let as1 = V.filter (\ab -> PM.absenceStart ab `I.member` interval) as0
    traverse (powerAbsence fpm) as1

powerAbsence
    :: MonadPlanMillQuery m
    => HashMap FUM.Login PM.User
    -> PM.Absence
    -> m PowerAbsence
powerAbsence idsLookup ab  = do
    let absenceInterval = PM.absenceInterval ab
    uc <- PMQ.capacities absenceInterval (PM.absencePerson ab)
    let uc' = capacities uc
    pure PowerAbsence
        { _powerAbsenceUsername     = HM.lookup (PM.absencePerson ab) revLookup
        , _powerAbsenceStart        = PM.absenceStart ab
        , _powerAbsenceEnd          = PM.absenceFinish ab
        , _powerAbsencePlanmillId   = ab ^. PM.identifier
        , _powerAbsenceCapacities   = uc'
        , _powerAbsenceBusinessDays = NDT $ length uc'
        }
  where
    revLookup :: HashMap PM.UserId FUM.Login
    revLookup
        = HM.fromList
        . map (first (view PM.identifier) . swap)
        . HM.toList
        $ idsLookup

    capacities :: PM.UserCapacities -> Map Day (NDT 'Hours Centi)
    capacities
        = Map.fromList
        . filter ((> 0) . snd)
        . map (\x -> (PM.userCapacityDate x, ndtConvert' $ PM.userCapacityAmount x))
        . toList
