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

data Task = Task
    { _taskId             :: !TaskId
    , taskName            :: !Text
    , taskProject         :: !(Maybe ProjectId) -- TODO: Sometimes unset?

    , taskStart           :: !Day
    , taskFinish          :: !Day

    -- what's the right type
    , taskTotalEffort    :: !Int
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
instance Structured Task

instance FromJSON Task where
    parseJSON = withObject "Task" $ \obj -> Task
        <$> (obj .: "id" <|> pure (Ident 0)) -- this is very BAD HACK
        <*> (getParsedAsText <$> obj .: "name" <|> pure "") -- HACK^2
        <*> obj .:? "project"
        <*> (dayFromZ <$> obj .: "start"  <|> pure (ModifiedJulianDay 57023)) -- 2015-01-01
        <*> (dayFromZ <$> obj .: "finish" <|> pure (ModifiedJulianDay 59580)) -- 2022-01-01
        <*> obj .:? "totalEffort" .!= 0
