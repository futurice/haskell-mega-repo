{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
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

import Control.Lens            ((<&>))
import Futurice.Email          (Email)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Office         (officeToText)
import Futurice.Prelude
import Futurice.Report.Columns
import Futurice.Tribe          (tribeToText)
import Prelude ()

import qualified Data.Map.Strict as Map
import qualified Data.Vector     as V
import qualified FUM
import qualified Personio        as P
import qualified PlanMill        as PM

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data PowerUser = PowerUser
    { _powerUserUsername           :: !FUM.Login
    , _powerUserPersonioId         :: !P.EmployeeId
    , _powerUserFirst              :: !Text
    , _powerUserLast               :: !Text
    , _powerUserTeam               :: !Text
    , _powerUserCompetence         :: !Text
    , _powerUserSupervisor         :: !(Maybe FUM.Login)
    , _powerUserSupervisorName     :: !(Maybe Text)
    , _powerUserStart              :: !(Maybe Day)
    , _powerUserEnd                :: !(Maybe Day)
    , _powerUserActive             :: !Text
    , _powerUserInternal           :: !Bool
    , _powerUserImpactRoles        :: ![Text]
    , _powerUserMatrixSupervisorName :: !(Maybe Text)
    , _powerUserInvoiceableFTE     :: !(Maybe Double)
    , _powerUserCompetenceHome     :: !(Maybe Text)
    , _powerUserPosition           :: !(Maybe Text)
    , _powerUserJobOfferAccepted   :: !(Maybe Day)
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

type PowerUserReport = Vector PowerUser

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

type EmployeeMap = Map P.EmployeeId P.Employee
type EmailMap = Map Email P.Employee

powerUserReport :: (PM.MonadTime m, MonadPersonio m) => m PowerUserReport
powerUserReport = do
    today <- currentDay
    es <- personio P.PersonioEmployees
    let es' = Map.fromList $ map (\e -> (e ^. P.employeeId, e)) es
    let emailMap = Map.fromList
                   $ catMaybes
                   $ map (\e -> (, e) <$> (e ^. P.employeeEmail)) es
    return $ V.fromList $ mapMaybe (powerUser today es' emailMap) es

powerUser :: Day -> EmployeeMap -> EmailMap -> P.Employee -> Maybe PowerUser
powerUser today es emailMap e = do
    login <- e ^. P.employeeLogin
    pure PowerUser
        { _powerUserUsername       = login
        , _powerUserPersonioId     = e ^. P.employeeId
        , _powerUserFirst          = e ^. P.employeeFirst
        , _powerUserLast           = e ^. P.employeeLast
        , _powerUserTeam           = team
        , _powerUserCompetence     = e ^. P.employeeRole
        , _powerUserStart          = e ^. P.employeeHireDate
        , _powerUserEnd            = e ^. P.employeeEndDate
        , _powerUserActive         =
            if employeeIsActive today e
            then "Active"
            else "Passive"
        , _powerUserSupervisor     = s >>= view P.employeeLogin
        , _powerUserSupervisorName = s <&> view P.employeeFullname
        , _powerUserInternal       = e ^. P.employeeEmploymentType == Just P.Internal
        , _powerUserImpactRoles    = e ^. P.employeeImpactRoles
        , _powerUserMatrixSupervisorName  = e ^. P.employeeMatrixSupervisorEmail
                                            >>= flip Map.lookup emailMap
                                            >>= \su -> Just $ su ^. P.employeeFullname
        , _powerUserInvoiceableFTE = e ^. P.employeeInvoiceableFTE
        , _powerUserCompetenceHome = e ^. P.employeeCompetenceHome
        , _powerUserPosition       = e ^. P.employeePosition
        , _powerUserJobOfferAccepted = e ^. P.employeeJobOfferAccepted
        }
  where
    s = do
        sid <- e ^. P.employeeSupervisorId
        es ^? ix sid
    tribeName = tribeToText $ e ^. P.employeeTribe
    team =
        case (tribeName, officeToText $ e ^. P.employeeOffice) of
            ("Germany", "Munich") -> "Munich"
            ("Germany", "Berlin") -> "Berlin"
            ("Germany", "Stuttgart") -> "Stuttgart"
            _ -> tribeName


-- | Employee is active if
--
-- * contacts end date isn't passed
--
-- * it's status is not 'Inactive'
--
employeeIsActive :: Day -> P.Employee -> Bool
employeeIsActive today e =
    maybe True (today <=) (P._employeeEndDate e)
    && (P._employeeStatus e /= P.Inactive)
