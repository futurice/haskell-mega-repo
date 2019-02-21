module Futurice.App.Schedule.Transactor where

import Control.Concurrent.MVar.Lifted (withMVar)
import Control.Concurrent.STM         (atomically, readTVar, writeTVar)
import Data.Aeson                     (toJSON)
import FUM.Types.Login
import Futurice.Lomake
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Stricter
import Prelude ()

import Futurice.App.Schedule.Command
import Futurice.App.Schedule.Command.Definition
import Futurice.App.Schedule.Ctx

import qualified Database.PostgreSQL.Simple as Postgres

-- | Write command to 'ctxCommandChananel', and wait for the reply.
--
-- | We want only own writer (i.e. not multiple HTTP-request threads) to
-- ensure consistency of written commands.
transact
    :: Ctx
    -> UTCTime       -- ^ now
    -> Login         -- ^ submitter of the command
    -> SomeCommand   -- ^ command
    -> LogT IO (Either String (CommandResponse ()))
transact ctx now login scmd =
    withSomeCommand scmd $ \_cmdTag cmd ->
    withMVar (ctxTransactorMVar ctx) $ \_  -> do
        -- logTrace ("command " <> commandTag (Proxy :: Proxy cmd)) cmd
        world <- liftIO $ atomically $ readTVar (ctxWorld ctx)
        case runStricterT (applyCommand now login cmd <* validateWorld) world of
            Right (res, world') -> do
                -- insert into db
                _ <- poolExecute ctx insertQuery (login, now, commandTag' cmd, toJSON cmd)
                -- update in-memory state
                liftIO $ atomically $ do
                    writeTVar (ctxWorld ctx) world'
--                    writeTChan (ctxCommandChannel ctx) scmd
                pure (Right res)
            Left err -> pure (Left err)
  where
    insertQuery :: Postgres.Query
    insertQuery = fromString $ unwords
        [ "INSERT INTO schedule.commands"
        , "  (username, created, command, payload)"
        , "VALUES"
        , "  (?,?,?,?)"
        ]

validateWorld :: Monad m => m () -- TODO: finish this
validateWorld = pure ()
