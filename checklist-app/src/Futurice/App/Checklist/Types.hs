{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
-- | Types employee in checklist logic.
--
-- Currently missing:
--
-- * How to model construction and modification of 'CheckList'. We want to pick
--   only needed 'Task's based on initial 'Employee' data, and that information
--   should be configurable dynamically.
module Futurice.App.Checklist.Types (
    -- * Core types
    -- ** Employee / employee
    Employee(..),
    ContractType(..),
    -- ** Tasks
    Task(..),
    TaskRole(..),
    CheckResult(..),
    Checklist(..),
    TaskItem (..),
    AnnTaskItem (..),
    TaskAppliance(..),
    TaskComment(..),
    -- ** Wrappers
    Identifier(..),
    identifierToText,
    HasIdentifier (..),
    identifierText,
    Name (..),
    HasName (..),
    -- * Functions
    employeeTaskApplies,
    -- * Lenses
    -- ** Employee
    employeeFirstName, employeeLastName, employeeContractType, employeeOffice, employeeConfirmed,
    employeePhone, employeeContactEmail, employeeStartingDay, employeeSupervisor, employeeTribe,
    employeeInfo, employeeFUMLogin, employeeHRNumber, employeeChecklist, employeePersonio,
    -- ** ContractType
    _ContractType,
    _ContractTypePermanent, _ContractTypeExternal, _ContractTypeFixedTerm,
    _ContractTypePartTimer, _ContractTypeSummerWorker,
    -- ** Task
    taskName, taskInfo, taskPrereqs, taskRole, taskComment,
    -- ** CheckResult
    _CheckResultSuccess, _CheckResultMaybe, _CheckResultFailure,
    -- ** TaskRole
    _TaskRole,
    _TaskRoleIT, _TaskRoleHR, _TaskRoleSupervisor,
    taskRoleToText, taskRoleFromText,
    PerTaskRole (..),
    -- ** Checklist
    checklistName, checklistTasks,
    -- ** TaskItem
    _TaskItem,
    _TaskItemDone, _TaskItemTodo,
    -- ** AnnTaskItem
    annTaskItemTodo,
    annTaskItemComment,
    _AnnTaskItemDone, _AnnTaskItemTodo,
    -- * World
    World,
    emptyWorld,
    mkWorld,
    -- ** Lenses
    worldEmployees,
    worldArchive,
    worldTasks,
    worldLists,
    worldTaskItems,
    worldTaskItems',
    worldTasksSorted,
    worldTasksSortedByName,
    -- * Access
    AuthUser,
    authUserTaskRole,
    -- * Counters
    Counter (..),
    TodoCounter (..),
    toTodoCounter,
    -- * Re-exports
    module Futurice.Office,
    module Futurice.Tribe,
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.Office
import Futurice.Tribe

import Futurice.App.Checklist.Types.Basic
import Futurice.App.Checklist.Types.ContractType
import Futurice.App.Checklist.Types.Counter
import Futurice.App.Checklist.Types.Identifier
import Futurice.App.Checklist.Types.TaskItem
import Futurice.App.Checklist.Types.World
import Futurice.App.Checklist.Types.TaskAppliance
import Futurice.App.Checklist.Types.TaskComment
import Futurice.App.Checklist.Types.TaskRole

import qualified FUM.Types.Login as FUM (Login)

type AuthUser = (FUM.Login, TaskRole)

authUserTaskRole :: Lens' AuthUser TaskRole
authUserTaskRole = _2
