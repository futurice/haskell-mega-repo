{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Personio.Types.Employee where

-- Uncomment to get attribute hashmap
-- #define PERSONIO_DEBUG 1

import Data.Aeson.Compat
import Data.Fixed                (Centi)
import Data.Semigroup            (Min (..))
import Data.Time                 (zonedTimeToLocalTime)
import FUM.Types.Login           (Login)
import Futurice.Aeson
import Futurice.Company
import Futurice.CostCenter
import Futurice.Email            (Email)
import Futurice.Generics
import Futurice.IdMap            (HasKey (..))
import Futurice.Office
import Futurice.Prelude
import Futurice.Time
import Futurice.Tribe
import Numeric.Interval.NonEmpty (Interval, (...))
import Prelude ()

import Personio.Internal.Attribute
import Personio.Types.ContractType
import Personio.Types.EmployeeId
import Personio.Types.EmploymentType
import Personio.Types.SalaryType
import Personio.Types.Status

import qualified Chat.Flowdock.REST        as FD
import qualified GitHub                    as GH
import qualified Numeric.Interval.NonEmpty as Interval

import Personio.Types.Internal

-- | Employee structure. Doesn't contain sensitive information.
data Employee = Employee
    { _employeeId               :: !EmployeeId
    , _employeeFirst            :: !Text
    , _employeeLast             :: !Text
    , _employeeHireDate         :: !(Maybe Day)
    , _employeeEndDate          :: !(Maybe Day)
    , _employeeRole             :: !Text
    , _employeeEmail            :: !(Maybe Email)
    , _employeeWorkPhone        :: !(Maybe Text)
    , _employeeSupervisorId     :: !(Maybe EmployeeId)
    , _employeeLogin            :: !(Maybe Login)
    , _employeeTribe            :: !Tribe  -- ^ defaults to 'defaultTribe'
    , _employeeOffice           :: !Office  -- ^ defaults to 'OffOther'
    , _employeeEmployer         :: !Company -- ^ default so Futurice Oy, 'companyFuturiceOy'
    , _employeeCountry          :: !Country
    , _employeeCostCenter       :: !(Maybe CostCenter)
    , _employeeGithub           :: !(Maybe (GH.Name GH.User))
    , _employeeFlowdock         :: !(Maybe FD.UserId)
    , _employeeStatus           :: !Status
    , _employeeHRNumber         :: !(Maybe Int)
    , _employeeEmploymentType   :: !(Maybe EmploymentType)
    , _employeeContractType     :: !(Maybe ContractType)
    , _employeeSalaryType       :: !(Maybe SalaryType)
    , _employeeHomePhone        :: !(Maybe Text)
    , _employeeHomeEmail        :: !(Maybe Text)
    , _employeePosition         :: !(Maybe Text)  -- ^ aka "title", /TODO/: make own type and non-Maybe.
    , _employeeWeeklyHours      :: !(NDT 'Hours Centi)
    , _employeeExpat            :: !Bool
    , _employeeBirthday         :: !(Maybe Day)
    , _employeeJobOfferAccepted :: !(Maybe Day)
#ifdef PERSONIO_DEBUG
    , _employeeRest             :: !(HashMap Text Attribute)
#endif
    }
  deriving (Eq, Show, Generic)

-- | Employee is active if
--
-- * contacts end date isn't passed
--
-- * it's status is 'Acitve' or 'Leave'.
--
employeeIsActive :: Day -> Employee -> Bool
employeeIsActive today e =
    maybe True (today <=) (_employeeEndDate e)
    && (_employeeStatus e == Active || _employeeStatus e == Leave)

-- | /Note/: this considers only contract dates
employeeIsActiveInterval :: Interval Day -> Employee -> Bool
employeeIsActiveInterval interval e =
    case (_employeeHireDate e, _employeeEndDate e) of
        (Nothing, Nothing)    -> False
        (Just hire, Nothing)  -> (hire ... hire) Interval.<=? interval -- exists y in interval, hire <= y
        (Nothing, Just end)   -> (end  ... end)  Interval.>=? interval
        (Just hire, Just end) -> (hire ... end)  Interval.==? interval

makeLenses ''Employee
deriveGeneric ''Employee

-- | @first last@
employeeFullname :: Getter Employee Text
employeeFullname = getter $ \e -> _employeeFirst e <> " " <> _employeeLast e

instance NFData Employee
instance Hashable Employee

instance HasKey Employee where
    type Key Employee = EmployeeId
    key = employeeId

deriveVia [t| Arbitrary Employee `Via` Sopica Employee |]
deriveVia [t| ToJSON Employee    `Via` Sopica Employee |]
deriveVia [t| FromJSON Employee `Via` Sopica Employee |]

instance ToSchema Employee where declareNamedSchema = sopDeclareNamedSchema

parsePersonioEmployee :: Value -> Parser Employee
parsePersonioEmployee = withObjectDump "Personio.Employee" $ \obj -> do
    type_ <- obj .: "type"
    if type_ == ("Employee" :: Text)
    then obj .: "attributes" >>= parseEmployeeObject
    else fail $ "Not Employee: " ++ type_ ^. unpacked

parseEmployeeObject :: HashMap Text Attribute -> Parser Employee
parseEmployeeObject obj' = Employee
    <$> parseAttribute obj "id"
    <*> parseAttribute obj "first_name"
    <*> parseAttribute obj "last_name"
    <*> fmap2 zonedDay (parseAttribute obj "hire_date")
    <*> endDate
    <*> parseDynamicAttribute obj "Primary role"
    <*> optional (parseAttribute obj "email")
    <*> parseDynamicAttribute obj "Work phone"
    <*> fmap getSupervisorId (parseAttribute obj "supervisor")
    <*> optional (parseDynamicAttribute obj "Login name")
    <*> fmap (fromMaybe defaultTribe . getName) (parseAttribute obj "department")
    <*> fmap (fromMaybe offOther . getName) (parseAttribute obj "office")
    <*> (parseDynamicAttribute obj "Employer" <|> pure companyFuturiceOy)
    <*> (parseDynamicAttribute obj "Country/Managing Company" <|> pure countryFinland)
    <*> fmap (fmap getCostCenter' . listToMaybe) (parseAttribute obj "cost_centers")
    <*> fmap getGithubUsername (parseDynamicAttribute obj "Github")
    <*> fmap getFlowdockId (parseDynamicAttribute obj "Flowdock")
    <*> parseAttribute obj "status"
    <*> parseDynamicAttribute obj "(FI) HR number"
    <*> parseAttribute obj "employment_type"
    <*> optional (parseDynamicAttribute obj "Contract type")
    <*> optional (parseDynamicAttribute obj "Salary type")
    <*> parseDynamicAttribute obj "Private phone"
    <*> parseDynamicAttribute obj "Private email"
    <*> parseAttribute obj "position"
    <*> fmap getWeeklyHours (parseAttribute obj "weekly_working_hours")
    <*> fmap getExpat (parseDynamicAttribute obj  "Expat")
    <*> fmap2 zonedDay (parseDynamicAttribute obj "Birthday")
    <*> fmap2 zonedDay (parseDynamicAttribute obj "Job offer accepted")
#ifdef PERSONIO_DEBUG
    <*> pure obj' -- for employeeRest field
#endif
  where
    zonedDay = localDay . zonedTimeToLocalTime
    obj = mkAttributes obj'

    endDate = do
        a <- fmap (fmap zonedDay) (parseAttribute obj "contract_end_date")
        b <- fmap (fmap zonedDay) (parseAttribute obj "termination_date")
        return $ fmap getMin $ fmap Min a <> fmap Min b

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap
