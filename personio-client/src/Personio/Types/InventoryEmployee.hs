{-# LANGUAGE TemplateHaskell #-}
module Personio.Types.InventoryEmployee where

import FUM.Types.Login
import Futurice.Email
import Futurice.Generics
import Futurice.Office
import Futurice.Prelude
import Futurice.Tribe
import Prelude ()

import Personio.Types.Employee
import Personio.Types.EmployeeId
import Personio.Types.EmploymentType

data InventoryEmployee = InventoryEmployee
    { _ieId             :: !EmployeeId
    , _ieFirst          :: !Text
    , _ieLast           :: !Text
    , _ieEmail          :: !(Maybe Email)
    , _ieHRNumber       :: !(Maybe Int)
    , _ieTribe          :: !Tribe
    , _ieOffice         :: !Office
    , _ieEmploymentType :: !(Maybe EmploymentType)
    , _ieLogin          :: !(Maybe Login)
    , _ieHireDate       :: !(Maybe Day)
    }
    deriving (Show, Typeable, GhcGeneric, SopGeneric, HasDatatypeInfo)
    deriving (FromJSON, ToJSON) via Sopica InventoryEmployee

makeLenses ''InventoryEmployee

instance ToSchema InventoryEmployee where declareNamedSchema = sopDeclareNamedSchema

inventoryEmployeeFromPersonio :: [Employee] -> [InventoryEmployee]
inventoryEmployeeFromPersonio = map mk
  where
    mk :: Employee -> InventoryEmployee
    mk e = InventoryEmployee
        {  _ieId            = e ^. employeeId
        , _ieFirst          = e ^. employeeFirst
        , _ieLast           = e ^. employeeLast
        , _ieEmail          = e ^. employeeEmail
        , _ieHRNumber       = e ^. employeeHRNumber
        , _ieTribe          = e ^. employeeTribe
        , _ieOffice         = e ^. employeeOffice
        , _ieEmploymentType = e ^. employeeEmploymentType
        , _ieLogin          = e ^. employeeLogin
        , _ieHireDate       = e ^. employeeHireDate
        }
