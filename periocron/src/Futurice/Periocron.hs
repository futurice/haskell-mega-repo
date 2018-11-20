{-# LANGUAGE GADTs             #-}
{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
-- | Pediodic execution on @IO@ actions.
module Futurice.Periocron (
    spawnPeriocron,
    -- * Job
    Job,
    mkJob,
    -- * Intervals
    Intervals,
    every,
    shifted,
    -- * Options
    Options,
    defaultOptions,
    optionsInterval,
    ) where

import Control.Concurrent          (ThreadId, forkIO, threadDelay)
import Control.Concurrent.STM      (atomically)
import Control.Concurrent.STM.TSem (TSem, newTSem, signalTSem, waitTSem)
import Control.Exception.Lifted    (bracket)
import Data.Ratio                  ((%))
import Futurice.Metrics.RateMeter  (mark)
import Futurice.Prelude
import Prelude ()
import System.Timeout              (timeout)
import Text.Printf                 (printf)
import Control.Concurrent.Async (Async, async)

import qualified System.Clock                as Clock

-------------------------------------------------------------------------------
-- Internals
-------------------------------------------------------------------------------

liftedAsync :: (MonadBaseControl IO m, StM m a ~ a, NFData a) => m a -> m (Async (NF a))
liftedAsync action = liftBaseWith $ \runInIO -> async $ do
    x <- runInIO action
    evaluate (makeNF x)

getMonotonicClock :: MonadIO m => m TimeSpec
getMonotonicClock = liftIO $ Clock.getTime Clock.Monotonic

-------------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------------

data Options = Options
    { _optionsLogger         :: !Logger
    , _optionsInterval       :: !TimeSpec
    , _optionsConcurrentJobs :: !Integer
    }

-- | Create 'Options' object.
defaultOptions :: Logger -> Options
defaultOptions logger = Options
    { _optionsLogger         = logger
    , _optionsInterval       = Clock.fromNanoSecs $ truncate (60e9 :: Rational)
    , _optionsConcurrentJobs = 2
    }

-- | Set periocron heart beat interval. Default is 1 minute.
optionsInterval :: Lens' Options TimeSpec
optionsInterval = lens _optionsInterval $ \opts x -> opts { _optionsInterval = x }

-------------------------------------------------------------------------------
-- Intervals
-------------------------------------------------------------------------------

type Intervals = [NominalDiffTime]

every :: NominalDiffTime -> Intervals
every interval = iterate (+ interval) 0

shifted :: NominalDiffTime -> Intervals -> Intervals
shifted diff = map (+ diff)

-------------------------------------------------------------------------------
-- Job
-------------------------------------------------------------------------------

data Job = Job
    { jobLabel     :: !Text
    , jobAction    :: IO (Either SomeException (Maybe ()))
    , jobIntervals :: [TimeSpec]
    }

-- | Create job entry.
mkJob
    :: NFData a
    => Text       -- ^ job label
    -> IO a       -- ^ job action
    -> Intervals  -- ^ job intervals
    -> Job
mkJob lbl action intervals = Job
    { jobLabel     = lbl
    , jobAction    = (fmap . fmap . fmap) (const ()) $ tryDeep $ timeout execTime $ action
    , jobIntervals = ndtToTimeSpec <$> intervals
    }
  where
    execTime = 10 * 60 * 1000000 -- 10 minutes
    ndtToTimeSpec = fromInteger . truncate . (* 1000000000) . toRational

-------------------------------------------------------------------------------
-- WorkerJob
-------------------------------------------------------------------------------

-- | A job we keep in the state
data WorkerJob = WorkerJob !TSem !Text (IO (Either SomeException (Maybe ())))

mergeJobs :: TimeSpec -> [(Job, TSem)] -> [(TimeSpec, WorkerJob)]
mergeJobs now jobs = mergeOrdered $ mk <$> jobs
  where
    mk (Job lbl action intervals, tsem) =
        ((now +) <$> intervals, WorkerJob tsem lbl action)

-------------------------------------------------------------------------------
-- Worker
-------------------------------------------------------------------------------

-- | Create a thread with periocron daemon.
spawnPeriocron :: Options -> [Job] -> IO ThreadId
spawnPeriocron options jobs = do
    now   <- getMonotonicClock
    tsem  <- atomically $ newTSem $ fromInteger $ _optionsConcurrentJobs options
    jobs' <- for jobs $ \job -> do
        tsem' <- atomically $ newTSem 1
        pure (job, tsem')
    let workerJobs = mergeJobs now jobs'
    forkIO $ workerLoop options tsem workerJobs

type WorkerState = (Integer, TimeSpec, [(TimeSpec, WorkerJob)])

workerLoop :: Options -> TSem -> [(TimeSpec, WorkerJob)] -> IO ()
workerLoop options tsem js = do
    now <- getMonotonicClock
    iterateM go (0, now - _optionsInterval options, js)
  where
    go :: WorkerState -> IO WorkerState
    go (jobCounter, prev, jobs) = runLogT "periocron" (_optionsLogger options) $ do
        now <- getMonotonicClock
        let (todo, rest) = span ((< now) . fst) jobs

        logInfo_ $
            "Heart beat at " <> textShow now <>
            "; jobs to do: " <> textShow (length todo)

        -- Execute jobs
        traverse_ (uncurry executeJob) $ zip [jobCounter..] $ map snd todo
        let jobCounter' = jobCounter + fromIntegral (length todo)

        -- Sleep
        let delay = _optionsInterval options - (now - prev - _optionsInterval options)
        let delay' = max 0 $ Clock.toNanoSecs delay `div` 1000
        logInfo_ $ "Sleeping for " <> textShow delay' <> " microseconds"
        liftIO $ threadDelay $ fromInteger delay'

        -- Loop
        pure (jobCounter', now, rest)

    executeJob :: Integer -> WorkerJob -> LogT IO ()
    executeJob jobId (WorkerJob tsem' label action) =
        void $ liftedAsync $ do
            logLocalDomain ("job-" <> textShow jobId) $
                bracket enter exit $ \_ -> do
                    x <- liftIO $ action >>= evaluate
                    case x of
                        Right (Just ()) -> pure ()
                        Right Nothing -> do
                            liftIO $ mark "Periocron job timeout"
                            logAttention_ $ "Timeout"
                        Left exc -> do
                            liftIO $ mark "Periocron exception"
                            logAttention_ $ "Exception -- " <> textShow exc
      where
        enter = do
            logInfo_ $ "Queueing '" <> label <> "'"
            liftIO $ atomically $ do
                waitTSem tsem'
                waitTSem tsem
            logInfo_ $ "Executing '" <> label <> "'"
            getMonotonicClock
        exit startTime = do
            endTime <- getMonotonicClock
            let d = Clock.diffTimeSpec endTime startTime
            let d' = fromRational $ Clock.toNanoSecs d % 1000000000 :: Double
            logInfo ("Done '" <> label <> "'; took " <> printf "%.06f" d' ^. packed <> "s") d
            liftIO $ atomically $ do
                signalTSem tsem
                signalTSem tsem'

-------------------------------------------------------------------------------
-- Merge helpers
-------------------------------------------------------------------------------

mergeOrdered :: Ord a => [([a], b)] -> [(a,b)]
mergeOrdered defs = mergeManyOn fst defs'
  where
    defs' = map t defs
    t (as, b) = map (,b) as

mergeOn :: forall a b. Ord b => (a -> b) -> [a] -> [a] -> [a]
mergeOn f xs ys = go xs' ys'
  where
    xs' = map f' xs
    ys' = map f' ys
    f' x = (x, f x)

    go :: [(a, b)] -> [(a, b)] -> [a]
    go [] bs = map fst bs
    go as [] = map fst as

    go as@((a, a') : as') bs@((b, b') : bs')
        | a' < b'   = a : go as' bs
        | otherwise = b : go as  bs'

mergeManyOn :: forall a b f. (Foldable f, Ord b) => (a -> b) -> f [a] -> [a]
mergeManyOn f = foldr (mergeOn f) []
