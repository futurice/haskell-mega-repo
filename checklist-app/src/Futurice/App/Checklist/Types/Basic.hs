{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
-- | Basic types
module Futurice.App.Checklist.Types.Basic where

import Futurice.Arbitrary (arbitraryAdjective, arbitraryNoun, arbitraryVerb)
import Futurice.Email     (Email)
import Futurice.Generics
import Futurice.Graph     (IsNode (..))
import Futurice.IdMap     (HasKey (..))
import Futurice.Office
import Futurice.Prelude
import Futurice.Tribe
import Prelude ()
import Servant.Graph      (ToDotVertex (..))

import Futurice.App.Checklist.Types.ChecklistId
import Futurice.App.Checklist.Types.ContractType
import Futurice.App.Checklist.Types.Identifier
import Futurice.App.Checklist.Types.TaskAppliance
import Futurice.App.Checklist.Types.TaskRole
import Futurice.App.Checklist.Types.TaskTag

import qualified Algebra.Graph.Export.Dot as Dot
import qualified Data.Map                 as Map
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as LT
import qualified FUM.Types.Login          as FUM
import qualified GitHub                   as GH
import qualified Okta                     as O
import qualified Personio                 as P
import qualified PlanMill                 as PM
import qualified Test.QuickCheck          as QC

newtype Name a = Name Text
  deriving (Eq, Ord, Show, Typeable, Generic)

instance FromJSON (Name a) where
    parseJSON v = Name <$> parseJSON v

instance ToJSON (Name a) where
    toJSON (Name n) = toJSON n

instance  ToHtml (Name a) where
    toHtml (Name a)    = toHtml a
    toHtmlRaw (Name a) = toHtmlRaw a

type TaskId = Identifier Task
type EmployeeId = Identifier Task

-- | All checklist tasks are tied to the employee
--
-- /TODO:/ add more fields? Is 'Employee' better name?
data Employee = Employee
    { _employeeId           :: !(Identifier Employee)
    , _employeeChecklist    :: !ChecklistId
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
    , _employeeNotice       :: !(Maybe Bool) -- employee with some special case which needs to be noticed
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData Employee

-- | 'Task' describes a particular task needs to be done. For example /"add to fum"/ or /"order a laptop".
data Task = Task
    { _taskId            :: !(Identifier Task)
    , _taskName          :: !(Name Task)
      -- ^ Display name
    , _taskInfo          :: !Text
      -- ^ additional info
    , _taskPrereqs       :: !(Set :$ Identifier Task)
      -- ^ Some tasks can be done only after some other tasks are done.
    , _taskRole          :: !TaskRole
      -- ^ Tasks can be fullfilled by different roles.
    , _taskComment       :: !Bool
      -- ^ Whether we render a comment field for this task.
    , _taskTags          :: !(Set TaskTag)
      -- ^ Task tags, "type" of it
    , _taskOffset        :: !Integer
      -- ^ Days offset for task: negative in advance, positive in the future
    , _taskApplicability :: !TaskAppliance
      -- ^ Who this task is applicable for
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

data TaskNode = TaskNode
    { _taskNodeId     :: !(Identifier Task)
    , _taskNodeName   :: !(Name Task)
    , _taskNodeOffset :: !Integer
    }
  deriving (Show, Typeable, Generic)

instance Eq TaskNode where
    (==) = (==) `on` _taskNodeId

instance Ord TaskNode where
    compare = compare `on` _taskNodeId

instance ToDotVertex TaskNode where
    exportVertexStyle = (Dot.defaultStyle render')
        { Dot.defaultVertexAttributes =
            [  "shape" Dot.:= "box"
            ]
        , Dot.graphAttributes =
            [ "rankdir" Dot.:= "LR"
            ]
        , Dot.edgeAttributes = ea
        }
      where
        render t = coerce (_taskNodeName t) <> " (" <> textShow (_taskNodeOffset t) <> ")"
        render' = LT.fromStrict . T.concatMap escape . render

        ea x y
            | _taskNodeOffset x > _taskNodeOffset y =
                [ "color" Dot.:= "red"
                ]
            | otherwise = []

        escape '"'  = "\\\""
        escape '\\' = "\\\\"
        escape c    = T.singleton c

-- Convert full 'Task' into 'TaskNode' (for graphs)
taskNode :: Task -> TaskNode
taskNode t = TaskNode
    { _taskNodeId     = _taskId t
    , _taskNodeName   = _taskName t
    , _taskNodeOffset = _taskOffset t
    }

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
    { _checklistId    :: !ChecklistId
    , _checklistTasks :: !(Set TaskId)
    }
  deriving (Eq, Ord, Show, Typeable, Generic)
-- | Helper structure for carrying Planmill user
data PMUser = PMUser
    { pmUser     :: !PM.User
    , pmPassive  :: !Text
    }
  deriving (Show, Generic)

instance NFData PMUser

-- | Helper structure for carrying data from external sources
data IntegrationData = IntegrationData
    { _githubData     :: !(Map (GH.Name GH.User) GH.SimpleUser)
    , _personioData   :: !(Map P.EmployeeId P.Employee)
    , _planmillData   :: !(HashMap FUM.Login (P.Employee, PMUser))
    , _oktaGithubData :: !(Map.Map Email (Maybe (GH.Name GH.User)))
    , _oktaUsers      :: !(Map.Map Text O.User)
    }
  deriving (Show, Generic)

instance NFData IntegrationData

-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------

makeWrapped ''Name
makeLenses ''Employee
makeLenses ''Task
makePrisms ''CheckResult
makeLenses ''Checklist
makeLenses ''IntegrationData

checklistIdName :: Getter ChecklistId (Name Checklist)
checklistIdName = getter $ \cid -> case cid of
    NewEmployeeChecklist              -> Name "New employee"
    LeavingEmployeeChecklist          -> Name "Leaving employee"
    FromInternalToExternalChecklist   -> Name "From Internal to External"
    FromFuturiceToSubsidiaryChecklist -> Name "From Futurice to Subsidiary"

checklistName :: Getter Checklist (Name Checklist)
checklistName = checklistId . checklistIdName

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
    type Key Checklist = ChecklistId
    key = checklistId


instance HasIdentifier Employee  Employee  where identifier = key
instance HasIdentifier Task      Task      where identifier = key

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

deriveVia [t| Arbitrary Employee  `Via` Sopica Employee |]
deriveVia [t| Arbitrary Checklist `Via` Sopica Checklist |]
deriveVia [t| Arbitrary Task      `Via` Sopica Task |]
