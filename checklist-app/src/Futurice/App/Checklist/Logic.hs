{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Logic (
    applyCommand,
    transactCommand,
    ) where

import Control.Lens               (forOf_, non, use, contains, preuse)
import Control.Monad.State.Strict (State, execState)
import Futurice.Prelude
import Prelude ()

import qualified Control.Lens               as Lens
import qualified Data.Map                   as DM
import qualified Database.PostgreSQL.Simple as Postgres
import qualified FUM.Types.Login            as FUM

import Futurice.App.Checklist.Command
import Futurice.App.Checklist.Types

-- | = operators are the same as ~ lens operators, but modify the state of MonadState.
--
-- todo: in error monad, if e.g. identifier don't exist
applyCommand :: UTCTime -> FUM.Login -> Command Identity -> World -> World
applyCommand now ssoUser cmd world = flip execState world $ case cmd of
    CmdCreateTask (Identity tid) (TaskEdit (Identity n) (Identity i) (Identity role) (Identity pr) (Identity comment) (Identity t) (Identity off) (Identity app)) ls -> do
        worldTasks . at tid ?= Task tid n i pr role comment t off app
        for_ ls $ \cid -> addTask cid tid

    CmdAddTask cid tid -> addTask cid tid

    CmdRemoveTask cid tid -> do
        worldLists . ix cid . checklistTasks . contains tid Lens..= False

        -- Remove this task from employee, if not already done
        es <- toList <$> use worldEmployees
        for_ es $ \e -> do
            let eid = e ^. identifier
            when (e ^. employeeChecklist == cid) $
                worldTaskItems . at eid . non mempty . at tid %= removeTodoTask

    CmdEditTask tid te -> do
        worldTasks . ix tid %= applyTaskEdit te
        toggleTasks tid

    CmdCreateEmployee (Identity eid) cid x -> do
        -- create user
        let e = fromEmployeeEdit eid cid x
        worldEmployees . at eid ?= e
        -- add initial tasks
        forOf_ (worldLists . ix cid . checklistTasks . folded) world $ \tid -> do
            mtask <- preuse $ worldTasks . ix tid
            for_ mtask $ \task -> when (employeeTaskApplies e $ task ^. taskApplicability) $
                worldTaskItems . at eid . non mempty . at tid ?= annTaskItemTodo

    CmdEditEmployee eid x -> do
        worldEmployees . ix eid %= applyEmployeeEdit x

    CmdTaskItemToggle eid tid d -> do
        let d' = case d of
                TaskItemTodo -> annTaskItemTodo
                TaskItemDone -> AnnTaskItemDone "" ssoUser now
        worldTaskItems . ix eid . ix tid Lens..= d'

    CmdTaskEditComment eid tid (TaskComment c) -> do
        worldTaskItems . ix eid . ix tid . annTaskItemComment Lens..= c

    -- TODO: differentiate between archiving and deleting. Now we delete.
    CmdArchiveEmployee eid Remove -> do
        worldTaskItems . at eid Lens..= Nothing
        worldEmployees . at eid Lens..= Nothing

    CmdArchiveEmployee eid Archive -> do
        let tasks = world ^. worldTaskItems . ix eid

        employee <- use (worldEmployees . at eid)
        worldArchive . at eid Lens..= (flip ArchivedEmployee (DM.map (\x ->
            case x of
              AnnTaskItemDone {} -> TaskItemDone
              AnnTaskItemTodo {} -> TaskItemTodo) tasks) <$> employee)

        worldTaskItems . at eid Lens..= Nothing
        worldEmployees . at eid Lens..= Nothing

  where
    -- tasks are added with both explicit CmdAddTask and during CmdCreateTask
    addTask :: ChecklistId -> TaskId -> State World ()
    addTask cid tid = do
        -- check that task exists
        mtask <- preuse $ worldTasks . ix tid
        for_ mtask $ \task -> do
            let app = task ^. taskApplicability

            -- add task to the checklist
            worldLists . ix cid . checklistTasks . contains tid Lens..= True

            -- for each employee
            es <- toList <$> use worldEmployees
            for_ es $ \e -> do
                let eid = e ^. identifier
                -- ... in the checklist
                when (e ^. employeeChecklist == cid) $ do
                    if employeeTaskApplies e app
                        -- if task applies, add it if not already there
                        then worldTaskItems . at eid . non mempty . at tid %= Just . fromMaybe annTaskItemTodo
                        -- if task doesn't apply, remove it if not yet done
                        else worldTaskItems . at eid . non mempty . at tid %= removeTodoTask

    -- when task is edited, we "re-add" it to all checklists it is in.
    -- This will force task to be added/removed when applicability changes.
    toggleTasks :: TaskId -> State World ()
    toggleTasks tid = for_ [minBound .. maxBound ] $ \cid -> do
        checklist <- use $ worldLists . pick cid
        when (checklist ^. checklistTasks . contains tid) $
            addTask cid tid

    removeTodoTask (Just (AnnTaskItemTodo _)) = Nothing
    removeTodoTask x                          = x

transactCommand
    :: (MonadLog m, MonadIO m)
    => Postgres.Connection -> FUM.Login -> Command Identity -> m ()
transactCommand conn ssoUser cmd = do
    logInfo "transactCommand" cmd
    _ <- liftIO $ Postgres.execute conn
        "INSERT INTO checklist2.commands (username, cmddata) VALUES (?, ?)"
        (ssoUser, cmd)
    pure ()
