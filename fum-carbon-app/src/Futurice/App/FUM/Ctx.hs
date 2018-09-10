{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Ctx (
    Ctx (..),
    newCtx,
    ctxMockUser,
    ) where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Concurrent.STM  (TChan, TVar, newBroadcastTChanIO, newTVarIO)
import Data.Pool               (Pool, createPool)
import Futurice.CryptoRandom   (CryptoGen, mkCryptoGen)
import Futurice.IdMap          (IdMap)
import Futurice.Postgres
       (HasPostgresPool (..), createPostgresPool, poolQuery_)
import Futurice.Prelude
import Futurice.Stricter       (StricterT, execStricterT)
import Prelude ()

import qualified Database.PostgreSQL.Simple as Postgres
import qualified Personio

import Futurice.App.FUM.Command
import Futurice.App.FUM.Config
import Futurice.App.FUM.Types

data Ctx = Ctx
    { ctxLogger         :: !Logger
    , ctxManager        :: !Manager
    , ctxConfig         :: !Config
    , ctxPersonio       :: !(TVar (IdMap Personio.Employee))
    , ctxPersonioData   :: !(TVar Personio.PersonioAllData)
    , ctxWorld          :: !(TVar World)
    , ctxTransactorMVar :: !(MVar ())
    , ctxCommandChannel :: !(TChan SomeCommand)
    , ctxPostgres       :: !(Pool Postgres.Connection)
    , ctxPRNGs          :: !(Pool (TVar CryptoGen))
    }

ctxMockUser :: Ctx -> Maybe Login
ctxMockUser = cfgMockUser . ctxConfig

instance HasPostgresPool Ctx where
    postgresPool = ctxPostgres

newCtx
    :: Logger
    -> Manager
    -> Config
    -> IdMap Personio.Employee
    -> Personio.PersonioAllData
    -> IO Ctx
newCtx lgr mgr cfg es vs = do
    pool <- createPostgresPool $ cfgPostgresConnInfo cfg
    cmds <- poolQuery_ pool selectQuery
    w <- case execStricterT (applyCommands cmds) emptyWorld of
        Right w  -> pure w
        Left err -> do
            runLogT "newCtx" lgr $ logAttention_ $ view packed err
            fail err
    Ctx lgr mgr cfg
        <$> newTVarIO es
        <*> newTVarIO vs
        <*> newTVarIO w
        <*> newMVar ()
        <*> newBroadcastTChanIO
        <*> pure pool
        <*> createPool (mkCryptoGen >>= newTVarIO) (\_ -> return()) 1 3600 5
  where
    selectQuery :: Postgres.Query
    selectQuery = fromString $ unwords
        [ "SELECT username, created, command, payload"
        , "FROM fumcarbon.commands"
        , "ORDER BY command_id ASC"
        ]

    applyCommands :: [(Login, UTCTime, Text, Value)] -> StricterT World (Either String) ()
    applyCommands [] = pure ()
    applyCommands ((login, now, tag, payload) : rest) = do
        scmd <- either fail pure $ decodeSomeCommand tag payload
        withSomeCommand scmd $ \_ cmd -> do
            _ <- applyCommand now login cmd
            validateWorld
            applyCommands rest
