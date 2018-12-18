{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futuqu.Rada.Tasks where

import Futurice.Generics
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified Data.Set         as Set
import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data Task = Task
    -- identifiers
    { tTaskId    :: !PM.TaskId
    , tProjectId :: !PM.ProjectId
    , tAccountId :: !PM.AccountId
    -- planmill
    , tName   :: !Text
    , tStart  :: !Day
    , tFinish :: !Day

    , tTotalEffort :: !Int
    }
  deriving stock (Eq, Ord, Show, GhcGeneric)
  deriving anyclass (NFData, SopGeneric, HasDatatypeInfo)

deriveVia [t| ToJSON Task         `Via` Sopica Task |]
deriveVia [t| FromJSON Task       `Via` Sopica Task |]
deriveVia [t| DefaultOrdered Task `Via` Sopica Task |]
deriveVia [t| ToRecord Task       `Via` Sopica Task |]
deriveVia [t| ToNamedRecord Task  `Via` Sopica Task |]

instance ToSchema Task where declareNamedSchema = sopDeclareNamedSchema

-------------------------------------------------------------------------------
-- Endpoint
-------------------------------------------------------------------------------

tasksData
    :: (MonadPlanMillQuery m, MonadMemoize m)
    => [PM.AccountId]
    -> [PM.ProjectId]
    -> m [Task]
tasksData fAccIds fProjIds = do
    prjs  <- PMQ.projects
    let ids :: [(PM.ProjectId, PM.AccountId)]
        ids = mapMaybe (\p -> (p ^. PM.identifier,) <$> PM.pAccount p) (toList prjs)
    mconcat <$> traverse fetch ids
  where
    fetch (prjId, accId)
        | not (predAccount accId && predProject prjId) =
            return []
        | otherwise = do
            tasks <- PMQ.projectTasks prjId
            return $ map (convert prjId accId) (toList tasks)

    convert prjId accId t = Task
        { tTaskId      = t ^. PM.identifier
        , tProjectId   = prjId
        , tAccountId   = accId
        , tName        = PM.taskName t
        , tStart       = PM.taskStart t
        , tFinish      = PM.taskFinish t
        , tTotalEffort = PM.taskTotalEffort t
        }

    predAccount accId = case fAccIds of
        [] -> True
        xs -> accId `elem` Set.fromList xs

    predProject projId = case fProjIds of
        [] -> True
        xs -> projId `elem` Set.fromList xs
