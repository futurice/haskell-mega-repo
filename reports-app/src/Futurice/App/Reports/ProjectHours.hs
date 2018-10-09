{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Futurice.App.Reports.ProjectHours (
    -- * Report
    projectHoursData,
    -- * Types
    ProjectHoursData,
    ProjectHours (..),
    ) where

import Control.Lens              (alaf)
import Data.Fixed                (Centi)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Futurice.Time
import Futurice.Time.Month
import Numeric.Interval.NonEmpty (Interval, (...))
import Prelude ()

import qualified Data.Map.Strict     as Map
import qualified FUM.Types.Login     as FUM
import qualified Personio            as P
import qualified PlanMill            as PM
import qualified PlanMill.Queries    as PMQ

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

-- | @PersonioID BillableHours NonBillableHours InternalWorkHours AbsenceHours Month MonthCapacity@
data ProjectHours = ProjectHours
    { phLogin            :: FUM.Login
    , phPersonio         :: P.EmployeeId
    , phBillableHours    :: !(NDT 'Hours Centi)
    , phNonBillableHours :: !(NDT 'Hours Centi)
    , phInternalHours    :: !(NDT 'Hours Centi)
    , phAbsenceHours     :: !(NDT 'Hours Centi)
    , phMonth            :: !Month
    , phMonthCapacity    :: !(NDT 'Hours Centi)
    }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

deriveGeneric ''ProjectHours
instance ToSchema ProjectHours where declareNamedSchema = sopDeclareNamedSchema
deriveVia [t| ToJSON ProjectHours   `Via` Sopica ProjectHours |]
deriveVia [t| FromJSON ProjectHours `Via` Sopica ProjectHours |]

newtype ProjectHoursData = PHD [ProjectHours]
  deriving newtype (Show, NFData, ToJSON, FromJSON)

deriveGeneric ''ProjectHoursData

instance ToSchema ProjectHoursData where declareNamedSchema = newtypeDeclareNamedSchema

-------------------------------------------------------------------------------
-- Fetch
-------------------------------------------------------------------------------

projectHoursData
    :: forall m. (MonadTime m, MonadPersonio m, MonadPlanMillQuery m)
    => m ProjectHoursData
projectHoursData = do
    today <- currentDay
    let interval = beginningOfPrev2Month today ... pred today
    fpm <- personioPlanmillMap
    PHD . fold <$> itraverse (\login (p, pm) -> perEmployee interval login p pm) fpm
  where
    perEmployee :: Interval Day -> FUM.Login -> P.Employee -> PM.User -> m [ProjectHours]
    perEmployee interval login p pmu  = do
        let uid = pmu ^. PM.identifier
        trs <- PMQ.timereports interval uid

        let grouped :: Map Month [PM.Timereport]
            grouped = Map.fromListWith (++)
                [ (dayToMonth (PM.trStart tr), [tr])
                | tr <- toList trs
                ]

        fmap toList $ ifor grouped $ \month trs' -> do
            uc <- PMQ.capacities (monthInterval month) uid
            Aux {..} <- groupHours trs'
            return ProjectHours
                { phLogin            = login
                , phPersonio         = p ^. P.employeeId
                , phBillableHours    = auxBillable
                , phNonBillableHours = auxNonBillable
                , phInternalHours    = auxInternal
                , phAbsenceHours     = auxAbsence
                , phMonth            = month
                , phMonthCapacity    = alaf Sum foldMap (ndtConvert' . PM.userCapacityAmount) uc
                }

    groupHours :: [PM.Timereport] -> m Aux
    groupHours trs = fmap mconcat $ for trs $ \tr -> do
        let hours = ndtConvert' $ PM.trAmount tr

        -- for some reason annual holidays (at least?) have unknown billable status
        billableStatus <- PMQ.enumerationValue (PM.trBillableStatus tr) "Non-billable"
        dutyType <- traverse (`PMQ.enumerationValue` "???") (PM.trDutyType tr)

        pid <- case PM.trProject tr of
            Just pid -> return (Just pid)
            Nothing  -> PM.taskProject <$> PMQ.task (PM.trTask tr)

        mproject <- traverse PMQ.project pid

        case status mproject billableStatus dutyType of
            Billable       -> return mempty { auxBillable    = hours }
            NonBillable    -> return mempty { auxNonBillable = hours }
            Internal       -> return mempty { auxInternal    = hours }
            Absence        -> return mempty { auxAbsence     = hours }
            BalanceAbsence -> return mempty

-------------------------------------------------------------------------------
-- Aux
-------------------------------------------------------------------------------

data Aux = Aux
    { auxBillable    :: NDT 'Hours Centi
    , auxNonBillable :: NDT 'Hours Centi
    , auxInternal    :: NDT 'Hours Centi
    , auxAbsence     :: NDT 'Hours Centi
    }

instance Semigroup Aux where
    Aux a b c d <> Aux a' b' c' d' =
        Aux (a + a') (b +  b') (c + c') (d + d')

instance Monoid Aux where
    mempty  = Aux 0 0 0 0
    mappend = (<>)

data Status = Billable | NonBillable | Internal | Absence | BalanceAbsence
  deriving (Eq, Show)

-- in hours-api we assume project-id 0 if tasks' project id is not set.
status
    :: Maybe PM.Project
    -> Text        -- ^ billable status
    -> Maybe Text  -- ^ duty type
    -> Status
status _        "Non-billable" (Just "Balance leave") = BalanceAbsence
status (Just p) "Non-billable" _  | isAbsence p       = Absence
status (Just p) "Non-billable" _  | isInternal p      = Internal
status _        "Non-billable" _                      = NonBillable
status _        _              _                      = Billable

-- | Absences go into magic project.
--
-- TODO: use enumeration
isAbsence :: PM.Project -> Bool
isAbsence p = PM.pCategory p == Just 900

-- | TODO: Use PM.Account with type == 100
-- or even better textual "My company"
isInternal :: PM.Project -> Bool
isInternal p = PM.pAccount p `elem`
    [ Just (PM.Ident i)
    | i <- [ 461507, 1003024, 3426, 17716, 469, 179108 ]
    ]

-------------------------------------------------------------------------------
-- Html
-------------------------------------------------------------------------------

instance ToHtml ProjectHoursData where
    toHtmlRaw = toHtml
    toHtml = toHtml . renderProjectHoursData

renderProjectHoursData :: ProjectHoursData -> HtmlPage "project-hours"
renderProjectHoursData (PHD xs) = page_ "Project Hours" $ do
    h1_ "Project hours"

    table_ $ do
        thead_ $ do
            th_ "Login"
            th_ "Personio"
            th_ "Month"
            th_ "Billable"
            th_ "Non-billable"
            th_ "Internal"
            th_ "Absence"
            th_ "Month Capacity"

        tbody_ $ for_ xs $ \ProjectHours {..} -> tr_ $ do
            td_ $ toHtml phLogin
            td_ $ toHtml phPersonio
            td_ $ toHtml $ show phMonth
            td_ $ toHtml phBillableHours
            td_ $ toHtml phNonBillableHours
            td_ $ toHtml phInternalHours
            td_ $ toHtml phAbsenceHours
            td_ $ toHtml phMonthCapacity
