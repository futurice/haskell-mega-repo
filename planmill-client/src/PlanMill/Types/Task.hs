{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.Task (
    Task(..),
    Tasks,
    TaskId,
    ) where

import PlanMill.Internal.Prelude

import PlanMill.Types.Identifier      (HasIdentifier (..), Identifier)
import PlanMill.Types.Project         (ProjectId)

type TaskId = Identifier Task
type Tasks = Vector Task


-- @TODO make more strict when schema known
data Task = Task
    { _taskId             :: !TaskId
    , taskName            :: !Text
    , taskBillableStatus  :: !(Maybe Int)
    , taskDescription     :: !(Maybe String)
    , taskDutyType        :: !(Maybe Int)
    , taskFinish          :: !UTCTime
    , taskFinishOld       :: !(Maybe UTCTime)
    , taskOriginalFinish  :: !(Maybe UTCTime)
    , taskOriginalStart   :: !(Maybe UTCTime)
    , taskParent          :: !(Maybe TaskId)
    , taskPredecessorTask :: !(Maybe Int)
    , taskPriceType       :: !(Maybe Int)
    , taskProject         :: !(Maybe ProjectId) -- TODO: Unset?
    , taskStart           :: !UTCTime
    -- , taskStatus          :: !(Maybe Int)
    -- TODO: In /project/:project_id/tasks returns as String,
    --       in /tasks/:task_id returns Int
    , taskTargetEffort    :: !(Maybe Int)
    , taskTempFinish      :: !(Maybe UTCTime)
    , taskType            :: !(Maybe Int) -- TODO: Task or Milestone
    , taskUnitPrice       :: !(Maybe Double) -- e.g. 104.4
    , taskWbs             :: !(Maybe String)
    }
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

makeLenses ''Task
deriveGeneric ''Task

instance HasKey Task where
    type Key Task = TaskId
    key = taskId

instance HasIdentifier Task Task where
    identifier = taskId

instance Hashable Task
instance NFData Task
instance AnsiPretty Task
instance Binary Task
instance HasStructuralInfo Task where structuralInfo = sopStructuralInfo
instance HasSemanticVersion Task

instance FromJSON Task where
    parseJSON = withObject "Task" $ \obj -> Task
        <$> obj .: "id"
        <*> (getParsedAsText <$> obj .: "name") -- HACK
        <*> obj .: "billableStatus"
        <*> obj .:? "description"
        <*> obj .: "dutyType"
        <*> (getU <$> obj .: "finish")
        <*> (getU <$$> obj .:? "finishOld")
        <*> (getU <$$> obj .:? "originalFinish")
        <*> (getU <$$> obj .:? "originalStart")
        <*> obj .:? "parent"
        <*> obj .:? "predecessorTask"
        <*> obj .:? "priceType"
        <*> obj .:? "project"
        <*> (getU <$> obj .: "start")
        -- <*> obj .: "status"
        <*> obj .:? "targetEffort"
        <*> (getU <$$> obj .:? "tempFinish")
        <*> obj .: "type"
        <*> obj .: "unitPrice"
        <*> obj .: "wbs"
