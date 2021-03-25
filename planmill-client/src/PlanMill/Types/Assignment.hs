{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.Assignment (
    Assignment(..),
    Assignments,
    AssignmentId,
    ) where

import PlanMill.Internal.Prelude

import PlanMill.Types.Identifier (HasIdentifier (..), Identifier)
import PlanMill.Types.Task       (TaskId)
import PlanMill.Types.User       (TeamId, UserId)

type AssignmentId = Identifier Assignment
type Assignments = Vector Assignment

-- | 'Assignment' is a @User ∨ Team ~ Task@ relation.
--
-- In other words, whether user (themselves or as a part of a team)
-- can mark hours to a particular task.
data Assignment = Assignment
    { _aId                  :: !AssignmentId
    , aTask                 :: !TaskId
    , aTaskName             :: !Text
    , aPersonOrTeam         :: !(Either UserId TeamId)
    , aPersonOrTeamName     :: !Text
    , aActualAmount         :: !(Maybe Int)
    , aComment              :: !(Maybe Text)
    , aDeclined             :: !(Maybe Int)
    , aPlannedAmount        :: !(Maybe Int)
    , aProposedAmount       :: !(Maybe Int)
    , aProposedDeadline     :: !(Maybe Int)
    , aProposedEffortAmount :: !(Maybe Int)
    , aRemainingAmount      :: !(Maybe Int)
    , aReportedEffort       :: !(Maybe Int)
    , aRequest              :: !(Maybe Int)
    , aStatus               :: !(Maybe Int)
    , aTaskBillableStatus   :: !(Maybe Int)
    , aTaskFinish           :: !(Maybe UTCTime)
    , aTaskStart            :: !(Maybe UTCTime)
    , aTaskStatus           :: !(Maybe Text)
    , aTotalAmount          :: !(Maybe Int)
    , aUnitPrice            :: !(Maybe Int)
    }
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

makeLenses ''Assignment
deriveGeneric ''Assignment

instance HasIdentifier Assignment Assignment where
    identifier = aId

instance Hashable Assignment
instance NFData Assignment
instance AnsiPretty Assignment
instance Binary Assignment
instance Structured Assignment

instance FromJSON Assignment where
    parseJSON = withObject "Assignment" $ \obj ->
        Assignment <$> obj .: "id"
                   <*> obj .: "task"
                   <*> obj .: "taskName"
                   <*> ((fmap Left $ obj .: "personOrTeam") <|> (fmap Right $ obj .: "personOrTeam"))
                   <*> obj .: "personOrTeamName"
                   <*> obj .:? "actualAmount"
                   <*> obj .:? "comment"
                   <*> obj .:? "declined"
                   <*> obj .:? "plannedAmount"
                   <*> obj .:? "proposedAmount"
                   <*> obj .:? "proposedDeadline"
                   <*> obj .:? "proposedEffortAmount"
                   <*> obj .:? "remainingAmount"
                   <*> obj .:? "reportedEffort"
                   <*> obj .:? "request"
                   <*> obj .:? "status"
                   <*> obj .:? "taskBillableStatus"
                   <*> obj .:? "taskFinish"
                   <*> obj .:? "taskStart"
                   <*> obj .:? "taskStatus"
                   <*> obj .:? "totalAmount"
                   <*> obj .:? "unitPrice"
