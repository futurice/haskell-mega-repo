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
    Location(..),
    FUMLogin(..),
    -- ** Tasks
    Task(..),
    TaskRole(..),
    CheckResult(..),
    Checklist(..),
    TaskItemDone (..),
    TaskAppliance(..),
    -- ** Wrappers
    Identifier(..),
    identifierToText,
    HasIdentifier (..),
    Name (..),
    -- * Lenses
    -- ** Employee
    employeeFirstName, employeeLastName, employeeContractType, employeeLocation, employeeConfirmed,
    employeePhone, employeeContactEmail, employeeStartingDay, employeeSupervisor, employeeTribe,
    employeeInfo, employeeFUMLogin, employeeHRNumber, employeeChecklist,
    -- ** ContractType
    _ContractTypePermanent, _ContractTypeExternal, _ContractTypeFixedTerm,
    _ContractTypePartTimer, _ContractTypeSummerWorker,
    -- ** Location
    _LocHelsinki, _LocTampere, _LocBerlin, _LocLondon,
    _LocStockholm, _LocMunich, _LocOther,
    -- ** Task
    taskName, taskCanBeDone, taskDependencies, taskCheck, taskRole,
    -- ** CheckResult
    _CheckResultSuccess, _CheckResultMaybe, _CheckResultFailure,
    -- ** TaskRole
    _TaskRoleIT, _TaskRoleHR, _TaskRoleSupervisor,
    -- ** Checklist
    checklistName, checklistTasks,
    -- ** TaskItemDone
    _TaskItemDone, _TaskItemTodo,
    -- * HTML
    Page (..),
    -- * World
    World,
    mkWorld,
    -- ** Lenses
    worldEmployees,
    worldTasks,
    worldLists,
    worldTaskItems,
    worldUsers,
    worldTaskItems',
    ) where

import Futurice.App.Checklist.Types.Basic
import Futurice.App.Checklist.Types.Page
import Futurice.App.Checklist.Types.Identifier
import Futurice.App.Checklist.Types.World
