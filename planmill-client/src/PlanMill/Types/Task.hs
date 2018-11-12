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

import PlanMill.Types.Identifier (HasIdentifier (..), Identifier (..))
import PlanMill.Types.Project    (ProjectId)

type TaskId = Identifier Task
type Tasks = Vector Task


-- @TODO make more strict when schema known
data Task = Task
    { _taskId             :: !TaskId
    , taskName            :: !Text
    , taskBillableStatus  :: !(Maybe Int)
    , taskDescription     :: !(Maybe String)
    , taskDutyType        :: !(Maybe Int)
    , taskFinish          :: !Day
    , taskFinishOld       :: !(Maybe UTCTime)
    , taskOriginalFinish  :: !(Maybe UTCTime)
    , taskOriginalStart   :: !(Maybe UTCTime)
    , taskParent          :: !(Maybe TaskId)
    , taskPredecessorTask :: !(Maybe Int)
    , taskPriceType       :: !(Maybe Int)
    , taskProject         :: !(Maybe ProjectId) -- TODO: Unset?
    , taskStart           :: !Day
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
        <$> (obj .: "id" <|> pure (Ident 0)) -- this is very BAD HACK
        <*> (getParsedAsText <$> obj .: "name" <|> pure "") -- HACK^2
        <*> obj .:? "billableStatus"
        <*> obj .:? "description"
        <*> obj .:? "dutyType"
        -- sometimes finish and start are missing :(
        <*> (dayFromZ <$> obj .: "finish" <|> pure (ModifiedJulianDay 58849)) -- 2020-01-01
        <*> (getU <$$> obj .:? "finishOld")
        <*> (getU <$$> obj .:? "originalFinish")
        <*> (getU <$$> obj .:? "originalStart")
        <*> obj .:? "parent"
        <*> obj .:? "predecessorTask"
        <*> obj .:? "priceType"
        <*> obj .:? "project"
        <*> (dayFromZ <$> obj .: "start" <|> pure (ModifiedJulianDay 57023)) -- 2015-01-01
        -- <*> obj .: "status"
        <*> obj .:? "targetEffort"
        <*> (getU <$$> obj .:? "tempFinish")
        <*> obj .:? "type"
        <*> obj .: "unitPrice"
        <*> obj .: "wbs"
