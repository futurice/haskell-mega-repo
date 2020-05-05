{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
module Futuqu.Rada.People where

import Data.Fixed            (Centi)
import Data.Time             (addDays)
import FUM.Types.Login       (Login)
import Futurice.Company      (Company, Country)
import Futurice.CostCenter   (CostCenter)
import Futurice.Email        (Email)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Office       (Office)
import Futurice.Prelude
import Futurice.Time
import Futurice.Tribe        (Tribe)
import Prelude ()

import qualified Data.Map.Strict  as Map
import qualified Personio         as P
import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ
import qualified Power

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data Person = Person
    -- identifiers
    { pLogin    :: !(Maybe Login)
    , pPersonio :: !P.EmployeeId
    , pPlanmill :: !(Maybe PM.UserId) -- TODO: rename to pUserId ?
    -- TODO: add power internal ID?
    -- personio
    , pFirst          :: !Text
    , pLast           :: !Text
    , pHireDate       :: !(Maybe Day)
    , pEndDate        :: !(Maybe Day)
    , pRole           :: !Text
    , pEmail          :: !(Maybe Email)
    , pSupervisorId   :: !(Maybe P.EmployeeId)
    , pTribe          :: !Tribe
    , pOffice         :: !Office
    , pEmployer       :: !Company
    , pCountry        :: !(Maybe Country)
    , pCostCenter     :: !(Maybe CostCenter)
    , pStatus         :: !P.Status
    , pHRNumber       :: !(Maybe Int)
    , pEmploymentType :: !(Maybe P.EmploymentType)
    , pContractType   :: !(Maybe P.ContractType)
    , pSalaryType     :: !(Maybe P.SalaryType)
    , pPosition       :: !(Maybe Text)
    , pWeeklyHours    :: !(NDT 'Hours Centi)
    , pExpat          :: !Bool
    -- power
    , pUtzTarget  :: !(Maybe Int)
    }
  deriving stock (Eq, Ord, Show, GhcGeneric)
  deriving anyclass (NFData, SopGeneric, HasDatatypeInfo)

deriveVia [t| ToJSON Person         `Via` Sopica Person |]
deriveVia [t| FromJSON Person       `Via` Sopica Person |]
deriveVia [t| DefaultOrdered Person `Via` Sopica Person |]
deriveVia [t| ToRecord Person       `Via` Sopica Person |]
deriveVia [t| ToNamedRecord Person  `Via` Sopica Person |]

instance ToSchema Person where declareNamedSchema = sopDeclareNamedSchema

data SimplePerson = SimplePerson
    { spFirst :: !Text
    , spLast  :: !Text
    , spRole  :: !Text
    } deriving (Eq, Ord, Show, GhcGeneric, SopGeneric, HasDatatypeInfo, NFData, ToSchema)
      deriving (ToJSON, DefaultOrdered, ToNamedRecord) via (Sopica SimplePerson)

-------------------------------------------------------------------------------
-- Endpoint
-------------------------------------------------------------------------------

peopleData
    :: (MonadTime m, MonadPersonio m, MonadPlanMillQuery m, MonadPower m)
    => m [Person]
peopleData = do
    today <- currentDay

    -- Don't show end dates far enough in the future
    let amendEndDate (Just d) | d > addDays 31 today = Nothing
        amendEndDate md                              = md

    -- P.employeeIsActive is strict(er)
    let amendStatus False P.Active = P.Inactive
        amendStatus _     s        = s

    pes <- personio P.PersonioEmployees
    pms <- PMQ.users
    pos <- Power.powerPeople

    let pmsMap :: Map Login PM.User
        pmsMap = Map.fromList
            [ (login, pm)
            | pm <- toList pms
            , login <- toList $ PM.userLogin pm
            ]

    let posMap :: Map Login Power.Person
        posMap = Map.fromList
            [ (login, po)
            | po <- pos
            , let login = Power.personLogin po
            ]

    return $ sortOn pLast $ flip mapMaybe pes $ \pe -> do
        let mlogin = pe ^. P.employeeLogin

        -- don't show people who aren't starting
        let hireDate = pe ^. P.employeeHireDate
        guard $ maybe False (addDays 30 today >) hireDate

        return Person
            { pLogin    = mlogin
            , pPersonio = pe ^. P.employeeId
            , pPlanmill = mlogin >>= \login -> pmsMap ^? ix login . PM.identifier
            , pFirst          = pe ^. P.employeeFirst
            , pLast           = pe ^. P.employeeLast
            , pHireDate       = pe ^. P.employeeHireDate
            , pEndDate        = amendEndDate $ pe ^. P.employeeEndDate
            , pRole           = pe ^. P.employeeRole
            , pEmail          = pe ^. P.employeeEmail
            , pSupervisorId   = pe ^. P.employeeSupervisorId
            , pTribe          = pe ^. P.employeeTribe
            , pOffice         = pe ^. P.employeeOffice
            , pEmployer       = pe ^. P.employeeEmployer
            , pCountry        = pe ^. P.employeeCountry
            , pCostCenter     = pe ^. P.employeeCostCenter
            , pStatus         = amendStatus (P.employeeIsActive today pe) $ pe ^. P.employeeStatus
            , pHRNumber       = pe ^. P.employeeHRNumber
            , pEmploymentType = pe ^. P.employeeEmploymentType
            , pContractType   = pe ^. P.employeeContractType
            , pSalaryType     = pe ^. P.employeeSalaryType
            , pPosition       = pe ^. P.employeePosition
            , pWeeklyHours    = pe ^. P.employeeWeeklyHours
            , pExpat          = pe ^. P.employeeExpat
            , pUtzTarget  = mlogin >>= \login  -> posMap ^? ix login . getter Power.personUtzTarget
            }

--subset of people data
peopleDataSimple
    :: (MonadTime m, MonadPersonio m, MonadPlanMillQuery m, MonadPower m)
    => m [SimplePerson]
peopleDataSimple = (fmap . fmap) simplify peopleData
  where
   simplify emp = SimplePerson
       { spFirst = pFirst emp
       , spLast  = pLast emp
       , spRole  = pRole emp
       }
