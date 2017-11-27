{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
-- | Basic types
module Futurice.App.Checklist.Types.Basic where

import Futurice.Arbitrary (arbitraryAdjective, arbitraryNoun, arbitraryVerb)
import Futurice.Generics
import Futurice.Graph     (IsNode (..))
import Futurice.IdMap     (HasKey (..))
import Futurice.Office
import Futurice.Tribe
import Futurice.Prelude
import Prelude ()

import Futurice.App.Checklist.Types.ContractType
import Futurice.App.Checklist.Types.Identifier
import Futurice.App.Checklist.Types.TaskAppliance
import Futurice.App.Checklist.Types.TaskRole

import qualified Data.Text       as T
import qualified FUM.Types.Login as FUM
import qualified Test.QuickCheck as QC

import qualified Personio as P

newtype Name a = Name Text
  deriving (Eq, Ord, Show, Typeable, Generic)

instance FromJSON (Name a) where
    parseJSON v = Name <$> parseJSON v

instance ToJSON (Name a) where
    toJSON (Name n) = toJSON n

-- | All checklist tasks are tied to the employee
--
-- /TODO:/ add more fields? Is 'Employee' better name?
data Employee = Employee
    { _employeeId           :: !(Identifier Employee)
    , _employeeChecklist    :: !(Identifier Checklist)
    , _employeePersonio     :: !(Maybe P.EmployeeId)
    , _employeeFirstName    :: !Text
    , _employeeLastName     :: !Text
    , _employeeContractType :: !ContractType
    , _employeeOffice       :: !Office
    , _employeeConfirmed    :: !Bool
      -- ^ /Note:/ This is non-work email!
    , _employeeStartingDay  :: !Day
    , _employeeSupervisor   :: !Text -- TODO: FUM.Login
    , _employeeTribe        :: !Tribe
      -- ^ /Note:/ ATM this is free form text.
    , _employeeInfo         :: !Text
      -- ^ Free text comments about the employee.
    -- Data filled up later:
    , _employeePhone        :: !(Maybe Text)
    , _employeeContactEmail :: !(Maybe Text)
    , _employeeFUMLogin     :: !(Maybe FUM.Login)
    , _employeeHRNumber     :: !(Maybe Int) -- TODO: make a newtype for this
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData Employee

-- | 'Task' describes a particular task needs to be done. For example /"add to fum"/ or /"order a laptop".
data Task = Task
    { _taskId           :: !(Identifier Task)
    , _taskName         :: !(Name Task)
      -- ^ Display name
    , _taskInfo         :: !Text
      -- ^ additional info
    , _taskPrereqs      :: !(Set :$ Identifier Task)
      -- ^ Some tasks can be done only after some other tasks are done.
    , _taskRole         :: !TaskRole
      -- ^ Tasks can be fullfilled by different roles.
    , _taskComment      :: !Bool
      -- ^ Whether we render a comment field for this task.
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

-- |
data CheckResult
    = CheckResultSuccess
      -- ^ Everything is ok
    | CheckResultMaybe
      -- ^ Non definitive answer, but doesn't prevent from completing task. E.g. long cache time might make employee still invisible in FUM.
    | CheckResultFailure
      -- ^ Definitively not ok, task cannot be completed.
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)

-- | Checklist is collection of tasks. Used to group tasks together to create task instances together.
--  Example lists are "new full-time employee in Helsinki"
data Checklist = Checklist
    { _checklistId    :: !(Identifier Checklist)
    , _checklistName  :: !(Name Checklist)
    , _checklistTasks :: !(Map (Identifier Task) TaskAppliance)
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------

makeWrapped ''Name
makeLenses ''Employee
makeLenses ''Task
makePrisms ''CheckResult
makeLenses ''Checklist

-------------------------------------------------------------------------------
-- TaskAppliance helpers
-------------------------------------------------------------------------------

employeeTaskApplies :: Employee -> TaskAppliance -> Bool
employeeTaskApplies e ta = taskApplianceToPredicate ta
    (e ^. employeeContractType, e ^. employeeOffice)

-------------------------------------------------------------------------------
-- HasIdentifier instances
-------------------------------------------------------------------------------

instance HasKey Employee where
    type Key Employee = Identifier Employee
    key = employeeId

instance HasKey Task where
    type Key Task = Identifier Task
    key = taskId

instance IsNode Task where
    nodeNeighbors t = t ^.. taskPrereqs . folded

instance HasKey Checklist where
    type Key Checklist = Identifier Checklist
    key = checklistId


instance HasIdentifier Employee  Employee  where identifier = key
instance HasIdentifier Task      Task      where identifier = key
instance HasIdentifier Checklist Checklist where identifier = key

instance Entity Employee  where entityName _ = "Employee"
instance Entity Task      where entityName _ = "Task"
instance Entity Checklist where entityName _ = "Checklist"

-------------------------------------------------------------------------------
-- Some arbitraries
-------------------------------------------------------------------------------

class    HasName a         where name :: Getter a (Name a)
instance HasName Task      where name = taskName
instance HasName Checklist where name = checklistName
instance HasName Employee  where
    name = getter impl
      where
        impl e = Name $ _employeeFirstName e <> " " <> _employeeLastName e

class ArbitraryName a where
    arbitraryName :: QC.Gen (Name a)

instance ArbitraryName Task where
    arbitraryName = (\a b -> Name $ T.toTitle a <> " " <> b)
        <$> arbitraryVerb
        <*> arbitraryNoun

instance ArbitraryName Checklist where
    arbitraryName = (\a b -> Name $ T.toTitle a <> " " <> b)
        <$> arbitraryAdjective
        <*> arbitraryNoun

instance ArbitraryName a => Arbitrary (Name a) where
    arbitrary = arbitraryName

-------------------------------------------------------------------------------
-- instances
-------------------------------------------------------------------------------

deriveGeneric ''Employee
deriveGeneric ''Task
deriveGeneric ''CheckResult
deriveGeneric ''Checklist

instance Arbitrary Employee where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance Arbitrary Checklist where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance Arbitrary Task where
    arbitrary = sopArbitrary
    shrink    = sopShrink
