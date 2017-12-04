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
import Futurice.Integrations  (IntegrationsConfig)
import Futurice.Prelude
import Futurice.Servant       (Cache)
import Prelude ()

import qualified Data.Map                   as M
import qualified Database.PostgreSQL.Simple as Postgres
import qualified FUM.Types.Login            as FUM

import Futurice.App.Checklist.Command
import Futurice.App.Checklist.Logic
import Futurice.App.Checklist.Types

data Ctx = Ctx
    { ctxLogger          :: !Logger
    , ctxManager         :: !Manager
    , ctxCache           :: !Cache
    , ctxIntegrationsCfg :: !(IntegrationsConfig '[Proxy, Proxy, I, Proxy, Proxy, I])
    , ctxWorld           :: TVar World
    , ctxOrigWorld       :: World
    , ctxPostgres        :: Pool Postgres.Connection
    , ctxPRNGs           :: Pool (TVar CryptoGen)
    , ctxMockUser        :: !(Maybe FUM.Login)
    , ctxACL             :: TVar (Map FUM.Login TaskRole)
    }

newCtx
    :: Logger
    -> Manager
    -> Cache
    -> IntegrationsConfig '[Proxy, Proxy, I, Proxy, Proxy, I]
    -> Postgres.ConnectInfo
    -> Maybe FUM.Login
    -> World
    -> IO Ctx
newCtx lgr mgr cache cfg ci mockUser w = do
    Ctx lgr mgr cache cfg
        <$> newTVarIO w
        <*> pure w
        <*> createPool (Postgres.connect ci) Postgres.close 1 60 5
        <*> createPool (mkCryptoGen >>= newTVarIO) (\_ -> return()) 1 3600 5
        <*> pure mockUser
        <*> newTVarIO M.empty

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
