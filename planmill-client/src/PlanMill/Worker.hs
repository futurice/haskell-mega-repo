{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module PlanMill.Worker where

import Control.Concurrent             (ThreadId, forkFinally, killThread)
import Control.Concurrent.STM         (atomically)
import Control.Concurrent.STM.TBQueue
       (TBQueue, isFullTBQueue, newTBQueue, readTBQueue, writeTBQueue)
import Control.Concurrent.STM.TMVar
       (TMVar, newEmptyTMVar, putTMVar, readTMVar)
import Control.Exception
       (AsyncException (..), asyncExceptionFromException, throwIO)
import Control.Monad.Http             (runHttpT)
import Data.Aeson.Compat              (FromJSON)
import Futurice.CryptoRandom          (mkCryptoGen, runCRandTThrow')
import Futurice.Prelude
import Prelude ()

#if MIN_VERSION_stm(2,5,0)
import Control.Concurrent.STM.TBQueue (lengthTBQueue)
import Data.Aeson.Compat              (object, (.=))
#endif

import PlanMill.Eval  (evalPlanMill)
import PlanMill.Types

data Workers = Workers
    { workerChan    :: TBQueue Job
    , workerThreads :: [ThreadId]
    }

data Job where
    Job :: (FromJSON a, NFData a)
        => TMVar (Either SomeException a) -> PlanMill a -> Job

data QueueFullException = QueueFullException
  deriving stock Show
  deriving anyclass (Exception)

submitPlanMill
    :: (FromJSON a, NFData a)
    => Workers
    -> PlanMill a
    -> IO a
submitPlanMill ws pm = submitPlanMillE ws pm >>= either throwIO return

submitPlanMillE
    :: (FromJSON a, NFData a)
    => Workers
    -> PlanMill a
    -> IO (Either SomeException a)
submitPlanMillE (Workers q _) pm = do
    tmvarE <- atomically $ do
        tmvar <- newEmptyTMVar
        full <- isFullTBQueue q
        if full
        then return (Left QueueFullException)
        else do
            writeTBQueue q (Job tmvar pm)
            return (Right tmvar)

    -- wait.
    case tmvarE of
        Left e      -> return (Left (SomeException e))
        Right tmvar -> atomically (readTMVar tmvar)

workers
    :: Logger
    -> Manager
    -> Cfg
    -> [Text]   -- ^ names
    -> IO Workers
workers lgr mgr cfg names = do
    runLogT "workers" lgr $
        logInfo "Spawning workers" names
    q <- atomically (newTBQueue 1000) -- arbitrary size
    tids <- for names $ \name -> do
        g <- mkCryptoGen
        forkFinally (loop name q g) cleanup
    pure (Workers q tids)
  where
    cleanup :: Either SomeException () -> IO ()
    cleanup e = runLogT "workers" lgr $
        case first asyncExceptionFromException e of
            Left (Just ThreadKilled) -> logInfo_ "Thread killed"
            _                        -> logAttention "Thread died" (show e)

    loop name q g = do
        Job tmvar pm <- atomically (readTBQueue q)

        e <- tryDeep
            $ flip runHttpT mgr
            $ runLogT name lgr
            $ flip runReaderT cfg
            $ runCRandTThrow' g
            $ evalPlanMill pm

        case e of
            Left exc -> do
                atomically (putTMVar tmvar (Left exc))
                g' <- mkCryptoGen
                loop name q g'
            Right (a, g') -> do
                atomically (putTMVar tmvar (Right a))
                loop name q g'

closeWorkers :: Workers -> LogT IO ()
closeWorkers (Workers _q tids) = do
    logInfo_ "Closing workers"

#if MIN_VERSION_stm(2,5,0)
    n <- lift $ atomically $ lengthTBQueue _q
    logInfoI "Length of the work queue $n" $ object [ "n" .= n ]
#endif

    for_ tids $ \tid -> do
        lift (killThread tid)
