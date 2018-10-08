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

import Data.Tuple                (swap)
import Futurice.Email            (Email)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Lucid.Foundation
import Futurice.Time.Month
import Data.Fixed (Centi)
import Futurice.Prelude
import Futurice.Time
import Numeric.Interval.NonEmpty (Interval, inf, sup, (...))
import Prelude ()

import qualified Data.HashMap.Strict as HM
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
projectHoursData = return $ PHD $ return
    ProjectHours
        { phLogin            = $(FUM.mkLogin "xxxx")
        , phPersonio         = P.EmployeeId 0
        , phBillableHours    = 0
        , phNonBillableHours = 0
        , phInternalHours    = 0
        , phAbsenceHours     = 0
        , phMonth            = dayToMonth $(mkDay "2017-01-01")
        , phMonthCapacity    = 0
        }

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

        tbody_ $ for_ xs $ \(ProjectHours {..}) -> tr_ $ do
            td_ $ toHtml phLogin
            td_ $ toHtml phPersonio
            td_ $ toHtml $ show phMonth
            td_ $ toHtml phBillableHours
            td_ $ toHtml phNonBillableHours
            td_ $ toHtml phInternalHours
            td_ $ toHtml phAbsenceHours
            td_ $ toHtml phMonthCapacity
