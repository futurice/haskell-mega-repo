{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Personio.Types.SimpleEmployee where

import Futurice.Generics
import Futurice.IdMap            (HasKey (..))
import Futurice.Prelude
import Futurice.Tribe
import Numeric.Interval.NonEmpty (Interval, (...))
import Personio.Types.Status
import Prelude ()

import qualified Numeric.Interval.NonEmpty as I

import Personio.Types.EmployeeId

-- | Simple, smaller Employee structure.
--
-- It contains mostly data which changes over time.
-- For example, /email/ rarely does.
data SimpleEmployee = SimpleEmployee
    { _simpleEmployeeId       :: !EmployeeId
    , _simpleEmployeeHireDate :: !(Maybe Day)
    , _simpleEmployeeEndDate  :: !(Maybe Day)
    , _simpleEmployeeTribe    :: !Tribe  -- ^ defaults to 'defaultTribe'
    , _simpleEmployeeStatus   :: !Status
    }
  deriving (Eq, Show, Generic)

makeLenses ''SimpleEmployee
deriveGeneric ''SimpleEmployee

instance NFData SimpleEmployee
instance Hashable SimpleEmployee

instance HasKey SimpleEmployee where
    type Key SimpleEmployee = EmployeeId
    key = simpleEmployeeId

deriveVia [t| Arbitrary SimpleEmployee `Via` Sopica SimpleEmployee |]
deriveVia [t| ToJSON SimpleEmployee    `Via` Sopica SimpleEmployee |]
deriveVia [t| FromJSON SimpleEmployee `Via` Sopica SimpleEmployee |]

instance ToSchema SimpleEmployee where declareNamedSchema = sopDeclareNamedSchema

class HasSimpleEmployee e where
    simpleEmployee :: Lens' e SimpleEmployee

    employeeId :: Lens' e EmployeeId
    employeeId = simpleEmployee . simpleEmployeeId

    employeeHireDate :: Lens' e (Maybe Day)
    employeeHireDate = simpleEmployee . simpleEmployeeHireDate

    employeeEndDate :: Lens' e (Maybe Day)
    employeeEndDate = simpleEmployee . simpleEmployeeEndDate

    employeeTribe :: Lens' e Tribe
    employeeTribe = simpleEmployee . simpleEmployeeTribe

    employeeStatus :: Lens' e Status
    employeeStatus = simpleEmployee . simpleEmployeeStatus

instance HasSimpleEmployee SimpleEmployee where
    simpleEmployee = id

-------------------------------------------------------------------------------
-- "Logic"
-------------------------------------------------------------------------------

-- | Employee is active if
--
-- * contacts end date isn't passed
--
-- * it's status is 'Acitve' or 'Leave'.
--
employeeIsActive :: HasSimpleEmployee e => Day -> e -> Bool
employeeIsActive today e =
    employeeIsActive' today e && (status == Active || status == Leave)
  where
    status = e ^. employeeStatus

-- | Like 'employeeIsActive' but doesn't check status
employeeIsActive' :: HasSimpleEmployee e => Day -> e -> Bool
employeeIsActive' today e =
    maybe False (today >=) (e ^. employeeHireDate)
    && maybe True (today <=) (e ^. employeeEndDate)

-- | /Note/: this considers only contract dates
employeeIsActiveInterval :: HasSimpleEmployee e => Interval Day -> e -> Bool
employeeIsActiveInterval interval e =
    case (e ^. employeeHireDate, e ^. employeeEndDate) of
        (Nothing, Nothing)    -> False
        (Just hire, Nothing)  -> (hire ... hire) I.<=? interval -- exists y in interval, hire <= y
        (Nothing, Just end)   -> (end  ... end)  I.>=? interval
        (Just hire, Just end) -> (hire ... end)  I.==? interval

employeeIsActiveWholeInterval :: HasSimpleEmployee e => Interval Day -> e -> Bool
employeeIsActiveWholeInterval interval e =
    case (e ^. employeeHireDate, e ^. employeeEndDate) of
        (Nothing, Nothing)    -> False -- in theory should be True..., in practice
        (Just hire, Nothing)  -> hire <= I.inf interval
        (Nothing, Just end)   ->                           I.sup interval <= end
        (Just hire, Just end) -> hire <= I.inf interval && I.sup interval <= end

-- | Simpler check, checks only if the employee's end date is after the given interval
employeeEndsAfterInterval :: HasSimpleEmployee e => Interval Day -> e -> Bool
employeeEndsAfterInterval interval e = case e ^. employeeEndDate of
    Nothing -> True
    Just ed -> ed > I.sup interval
