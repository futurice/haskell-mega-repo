{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.ReportableAssignment (
    ReportableAssignment(..),
    ReportableAssignments,
    ) where

import PlanMill.Internal.Prelude

import PlanMill.Types.Assignment (Assignment, AssignmentId)
import PlanMill.Types.Identifier (HasIdentifier (..))
import PlanMill.Types.Project    (ProjectId)
import PlanMill.Types.Task       (TaskId)

type ReportableAssignments = Vector ReportableAssignment

data ReportableAssignment = ReportableAssignment
    { _raId                   :: !AssignmentId
    , raTask                  :: !TaskId
    , raTaskName              :: !Text
    , raProject               :: !ProjectId
    , raProjectName           :: !Text
    , raTaskStart             :: !Day
    , raTaskFinish            :: !Day
    , raLastTimereportCreated :: !UTCTime
    }
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

makeLenses ''ReportableAssignment
deriveGeneric ''ReportableAssignment

instance HasIdentifier ReportableAssignment Assignment where
    identifier = raId

instance Hashable ReportableAssignment
instance NFData ReportableAssignment
instance AnsiPretty ReportableAssignment
instance Binary ReportableAssignment
instance Structured ReportableAssignment

instance FromJSON ReportableAssignment where
    parseJSON = withObject "ReportableAssignment" $ \obj ->
        ReportableAssignment
            <$> obj .: "assignment"
            <*> obj .: "task"
            <*> obj .: "taskName"
            <*> obj .: "project"
            <*> obj .: "projectName"
            <*> (dayFromZ <$> obj .: "taskStart")
            <*> (dayFromZ <$> obj .: "taskFinish")
            <*> obj .: "lastTimereportCreated"
