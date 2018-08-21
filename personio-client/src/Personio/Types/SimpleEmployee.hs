{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
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

import qualified Numeric.Interval.NonEmpty as Interval

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
    maybe True (today <=) (e ^. employeeEndDate)
    && (status == Active || status == Leave)
  where
    status = e ^. employeeStatus

-- | /Note/: this considers only contract dates
employeeIsActiveInterval :: HasSimpleEmployee e =>Interval Day -> e -> Bool
employeeIsActiveInterval interval e =
    case (e ^. employeeHireDate, e ^. employeeEndDate) of
        (Nothing, Nothing)    -> False
        (Just hire, Nothing)  -> (hire ... hire) Interval.<=? interval -- exists y in interval, hire <= y
        (Nothing, Just end)   -> (end  ... end)  Interval.>=? interval
        (Just hire, Just end) -> (hire ... end)  Interval.==? interval
