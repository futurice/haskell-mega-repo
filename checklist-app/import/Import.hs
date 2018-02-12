{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
-- |
--
-- > tajna run -s checklist-import checklist-app/data.sample.yaml
module Main (main) where

import Algebra.Lattice                  (top)
import Control.Exception.Lifted         (bracket)
import Control.Lens                     (use, _4)
import Control.Monad.Trans.State.Strict
       (StateT (..), evalState, execStateT, modify')
import Data.Aeson.Compat
       (FromJSON (..), Parser, withObject, (.:))
import Data.Yaml                        (decodeFileEither)
import FUM.Types.Login                  (mkLogin)
import Futurice.EnvConfig               (getConfig)
import Futurice.Prelude
import Futurice.UUID                    (uuidWords)
import Prelude ()
import System.Environment               (getArgs)

import qualified Data.UUID                  as UUID
import qualified Database.PostgreSQL.Simple as Postgres

import Futurice.App.Checklist.Command
import Futurice.App.Checklist.Config
import Futurice.App.Checklist.Logic
import Futurice.App.Checklist.Types


-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fp] -> main' fp
        _    -> putStrLn "Usage: checklist-import data.yaml"

main' :: FilePath -> IO ()
main' fp = withStderrLogger $ \logger -> runLogT "checklist-import" logger $ do
    Config {..} <- getConfig "CHECKLIST"
    value <- liftIO $ decodeFileEither fp
    case value of
        Left exc -> liftIO $ putStrLn $ show exc
        Right v  -> do
            let cmds = evalState (dataToCommands v) UUID.nil
            bracket (liftIO $ Postgres.connect cfgPostgresConnInfo) (liftIO . Postgres.close) $ \conn -> do
                for_ cmds $ transactCommand conn $(mkLogin "xxxx")

-------------------------------------------------------------------------------
-- MonadUUID
-------------------------------------------------------------------------------

class Monad m => MonadUUID m where
    newUUID :: m UUID

instance (Monad m, HasUUID s) => MonadUUID (StateT s m) where
    newUUID = do
        uuid . uuidWords . _4 %= succ
        use uuid

-------------------------------------------------------------------------------
-- Data to commands
-------------------------------------------------------------------------------

dataToCommands :: forall m. MonadUUID m => Data -> m [Command Identity]
dataToCommands (Data cls ts) = ($ []) <$> execStateT action id
  where
    action :: StateT ([Command Identity] -> [Command Identity]) m ()
    action = do
        -- Tasks
        ts' <- for ts $ \(TaskD n r) -> do
            taskId <- Identifier <$> lift newUUID
            tellCmd $ CmdCreateTask (pure taskId) (TaskEdit (pure n) (pure "") (pure r) (pure mempty) (pure False) (pure mempty)) []
            pure taskId

        -- Checklists
        for_  cls $ \(ChecklistD n cts) -> do
            checklistId <- Identifier <$> lift newUUID
            tellCmd $ CmdCreateChecklist (pure checklistId) n

            for_ cts $ \td' -> case td' of
                TaskId taskId' -> do
                    taskId <- maybe (fail "no task") pure $ ts' ^? ix taskId'
                    tellCmd $ CmdAddTask checklistId taskId top
                TaskD' (TaskD n' r) -> do
                    taskId <- Identifier <$> lift newUUID
                    tellCmd $ CmdCreateTask (pure taskId) (TaskEdit (pure n') (pure "") (pure r) (pure mempty) (pure False) (pure mempty)) []
                    tellCmd $ CmdAddTask checklistId taskId top

    tellCmd :: Command pure -> StateT ([Command pure] -> [Command pure]) m ()
    tellCmd cmd = modify' (. (cmd :))


-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data Data = Data
    !(Map Text ChecklistD)
    !(Map Text TaskD)
  deriving (Show)

data ChecklistD = ChecklistD
    { _clName  :: !(Name Checklist)
    , _clTasks :: ![TaskD']
    }
  deriving (Show)

data TaskD'
    = TaskD' TaskD
    | TaskId Text
  deriving (Show)

data TaskD = TaskD
    { _tName :: !(Name Task)
    , _tRole :: !TaskRole
    }
  deriving (Show)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance FromJSON Data where
    parseJSON = withObject "Data" $ \obj -> Data
        <$> obj .: "checklists"
        <*> obj .: "tasks"

instance FromJSON ChecklistD where
    parseJSON = withObject "Checklist" $ \obj -> ChecklistD
        <$> obj .: "name"
        <*> obj .: "tasks"

instance FromJSON TaskD' where
    parseJSON v = TaskD' <$> parseJSON v <|> TaskId <$> parseId v
      where
        parseId :: Value -> Parser Text
        parseId = withObject "Task id" $ \obj -> obj .: "id"

instance FromJSON TaskD where
    parseJSON = withObject "Task" $ \obj -> TaskD
        <$> obj .: "name"
        <*> obj .: "role"
