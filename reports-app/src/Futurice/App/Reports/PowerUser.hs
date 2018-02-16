{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- | Missing hours report
module Futurice.App.Reports.PowerUser (
    -- * Report
    PowerUserReport,
    powerUserReport,
    -- * Types
    PowerUser (..),
    ) where

import Control.Lens (toListOf)
import Prelude ()
import Futurice.Prelude
import Futurice.Generics
import Futurice.Integrations
import Futurice.Report.Columns

import qualified Personio            as P
import qualified PlanMill            as PM
import qualified PlanMill.Queries    as PMQ
import qualified Data.HashMap.Strict as HM
import qualified FUM

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data PowerUser = PowerUser
    { _powerUserUsername       :: !FUM.Login
    , _powerUserFirst          :: !Text
    , _powerUserLast           :: !Text
    , _powerUserTeam           :: !Text
    , _powerUserCompetence     :: !Text
    , _powerUserSupervisor     :: !(Maybe FUM.Login)
    , _powerUserSupervisorName :: !(Maybe Text)
    , _powerUserStart          :: !(Maybe Day)
    , _powerUserEnd            :: !(Maybe Day)
    , _powerUserActive         :: !Text
    }
  deriving (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (NFData)

deriveGeneric ''PowerUser
deriveVia [t| ToJSON PowerUser   `Via` Sopica PowerUser |]
deriveVia [t| FromJSON PowerUser `Via` Sopica PowerUser |]
instance ToColumns PowerUser
instance ToSchema PowerUser where declareNamedSchema = sopDeclareNamedSchema

-- instance DefaultOrdered PowerUser where headerOrder = sopHeaderOrder
-- instance ToNamedRecord PowerUser where toNamedRecord = sopToNamedRecord

-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

type PowerUserReport = [PowerUser]

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

type SupervisorMap = HashMap P.EmployeeId P.Employee

powerUserReport
    :: forall m env.
        ( PM.MonadTime m, MonadFUM m, MonadPersonio m, MonadPlanMillQuery m
        , MonadReader env m, HasFUMEmployeeListName env
        )
    => m PowerUserReport
powerUserReport = do
    employees <- personio P.PersonioEmployees
    toListOf folded <$> itraverse
        (powerUser $ supervisors employees)
        (byLogin employees)
  where
    -- TODO: move into futurice-integrations, also used in balances
    supervisors :: [P.Employee] -> SupervisorMap
    supervisors = HM.fromList $ map (\e -> (e ^. P.employeeId, e))

powerUser
    :: MonadPlanMillQuery m
    => SupervisorMap
    -> FUM.Login
    -> P.Employee
    -> m PowerUser
powerUser supervisors fumLogin e = pure PowerUser
    { _powerUserUsername       = fumLogin
    , _powerUserFirst          = e ^. P.employeeFirst
    , _powerUserLast           = e ^. P.employeeLast
    , _powerUserTeam           = e ^. P.employeeTribe
    , _powerUserCompetence     = fromMaybe "No competence" (e ^. P.employeeCompetence)
    , _powerUserStart          = e ^. P.employeeHireDate e
    , _powerUserEnd            = e ^. P.employeeEndDate
    , _powerUserActive         = e ^. P.employeeStatus
    , _powerUserSupervisor     = supervisors ^? ix eid . P.employeeLogin . _Just
    , _powerUserSupervisorName = supervisors ^? ix eid . P.employeeFullname
    }
  where
    eid = e ^. P.employeeId
