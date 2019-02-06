{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Schedule.Ctx where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Concurrent.STM  (TVar, newTVarIO)
import Data.Pool               (Pool, createPool)
import FUM.Types.Login         (Login)
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Stricter       (StricterT, execStricterT)
import Prelude ()

import Futurice.App.Schedule.Command
import Futurice.App.Schedule.Command.Definition
import Futurice.App.Schedule.Config
import Futurice.App.Schedule.World

import qualified Database.PostgreSQL.Simple as Postgres

data Ctx = Ctx
    { ctxLogger         :: !Logger
    , ctxManager        :: !Manager
    , ctxPostgres       :: !(Pool Postgres.Connection)
    , ctxTransactorMVar :: !(MVar ())
    , ctxConfig         :: !Config
    , ctxWorld          :: !(TVar World)
    }

instance HasPostgresPool Ctx where
    postgresPool = ctxPostgres


newCtx :: Logger -> Manager -> Config -> IO Ctx
newCtx lgr mgr cfg = do
    pool <- createPostgresPool $ cfgPostgesConnInfo cfg
    cmds <- poolQuery_ pool selectQuery
    w <- case execStricterT (applyCommands cmds) emptyWorld of
        Right w  -> pure w
        Left err -> do
            runLogT "newCtx" lgr $ logAttention_ $ view packed err
            fail err
    worldTVar <- newTVarIO w
    mvar <- newMVar ()
    return $ Ctx lgr mgr pool mvar cfg worldTVar
  where
    selectQuery :: Postgres.Query
    selectQuery = fromString $ unwords
        [ "SELECT username, created, command, payload"
        , "FROM schedule.commands"
        , "ORDER BY command_id ASC"
        ]

    applyCommands :: [(Login, UTCTime, Text, Value)] -> StricterT World (Either String) ()
    applyCommands [] = pure ()
    applyCommands ((login, now, tag, payload) : rest) = do
        scmd <- either fail pure $ decodeSomeCommand tag payload
        withSomeCommand scmd $ \_ cmd -> do
            _ <- applyCommand now login cmd
--            validateWorld  -- implement this
            applyCommands rest
