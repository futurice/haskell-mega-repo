{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
module Futurice.App.Checklist.Ctx (
    Ctx (..),
    newCtx,
    ctxApplyCmd,
    ctxGetCRandom,
    -- * Helpers
    ctxWithCryptoGen,
    ) where

import Control.Concurrent.STM
       (TVar, atomically, modifyTVar', newTVarIO, readTVar, writeTVar)
import Data.Pool              (Pool, createPool, withResource)
import Futurice.CryptoRandom
       (CRandT, CRandom, CryptoGen, CryptoGenError, getCRandom, mkCryptoGen,
       runCRandT)
import Futurice.Email         (Email)
import Futurice.Integrations  (IntegrationsConfig)
import Futurice.Prelude
import Futurice.Servant       (Cache)
import Prelude ()

import qualified Data.Map                   as M
import qualified Database.PostgreSQL.Simple as Postgres
import qualified FUM.Types.Login            as FUM
import qualified GitHub                     as GH
import qualified Personio
import qualified Slack

import Futurice.App.Checklist.Command
import Futurice.App.Checklist.Config
import Futurice.App.Checklist.Logic
import Futurice.App.Checklist.Types

data Ctx = Ctx
    { ctxLogger          :: !Logger
    , ctxManager         :: !Manager
    , ctxCache           :: !Cache
    , ctxIntegrationsCfg :: !(IntegrationsConfig ChecklistIntegrations)
    , ctxWorld           :: TVar World
    , ctxOrigWorld       :: World
    , ctxPostgres        :: Pool Postgres.Connection
    , ctxPRNGs           :: Pool (TVar CryptoGen)
    , ctxMockUser        :: !(Maybe FUM.Login)
    , ctxACL             :: TVar (Map FUM.Login TaskRole)
    , ctxPersonio        :: TVar [Personio.Employee]
    , ctxOktaGithub      :: TVar (Map Email (Maybe (GH.Name GH.User)))
    , ctxSlackToken      :: Slack.SlackToken
    , ctxSlackChannel    :: Slack.ChannelId
    }

newCtx
    :: Logger
    -> Manager
    -> Cache
    -> IntegrationsConfig ChecklistIntegrations
    -> Postgres.ConnectInfo
    -> Maybe FUM.Login
    -> World
    -> Slack.SlackToken
    -> Slack.ChannelId
    -> IO Ctx
newCtx lgr mgr cache cfg ci mockUser w token channel = do
    Ctx lgr mgr cache cfg
        <$> newTVarIO w
        <*> pure w
        <*> createPool (Postgres.connect ci) Postgres.close 1 60 5
        <*> createPool (mkCryptoGen >>= newTVarIO) (\_ -> return()) 1 3600 5
        <*> pure mockUser
        <*> newTVarIO M.empty
        <*> newTVarIO []
        <*> newTVarIO M.empty
        <*> pure token
        <*> pure channel

ctxWithCryptoGen
    :: MonadIO m
    => Ctx -> CRandT CryptoGen CryptoGenError Identity a -> m a
ctxWithCryptoGen ctx m = liftIO $
    withResource (ctxPRNGs ctx) $ \tg -> atomically $ do
        g <- readTVar tg
        (x, g') <- either throwM pure $ runIdentity $ runCRandT m g
        writeTVar tg g'
        pure x

ctxGetCRandom :: (MonadIO m, CRandom a) => Ctx -> m a
ctxGetCRandom ctx = ctxWithCryptoGen ctx getCRandom

ctxApplyCmd
    :: (MonadLog m, MonadBaseControl IO m, MonadIO m)
    => UTCTime -> FUM.Login -> Command Identity -> Ctx -> m ()
ctxApplyCmd now fumuser cmd ctx = do
    liftIO $ atomically $ modifyTVar' (ctxWorld ctx) (applyCommand now fumuser cmd)
    withResource (ctxPostgres ctx) $ \conn ->
        transactCommand conn fumuser cmd
