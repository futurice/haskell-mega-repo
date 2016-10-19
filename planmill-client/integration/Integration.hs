{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Main (main) where

import Prelude ()
import Futurice.Prelude

import Control.Monad.Http                      (evalHttpT)
import Control.Monad.Logger
       (LogLevel (..), LogSource, filterLogger)
import Control.Monad.Reader                    (runReaderT)
import Data.Constraint
import Futurice.Constraint.Unit1
import Data.Maybe                              (isJust)
import System.Environment                      (getArgs, lookupEnv)
import System.IO                               (hPutStrLn, stderr)
import Text.PrettyPrint.ANSI.Leijen.AnsiPretty
       (AnsiPretty (..), linebreak, putDoc)

import qualified Data.Vector as V
import qualified Haxl.Core   as H

import           PlanMill
import qualified PlanMill.Types.Query  as Q
import qualified PlanMill.Queries      as Q
import           PlanMill.Queries.Haxl

main :: IO ()
main = do
    debug <- isJust <$> lookupEnv "DEBUG"
    args <- getArgs
    case args of
        [uid, apikey, endpoint] -> do
            let cfg = Cfg (Ident $ read uid) (ApiKey $ fromString apikey) endpoint
            haxlScript1 cfg
            integration debug cfg
        _                       ->
            hPutStrLn stderr "to run integration tests: ./integration uid apikey endpoint"

putPretty :: (MonadIO m, AnsiPretty a) => a -> m ()
putPretty = liftIO . putDoc . (<> linebreak) . ansiPretty

script1 :: (MonadPlanMill m, MonadIO m) => m ()
script1 = planmillAction me >>= putPretty

script2 :: (MonadPlanMill m, MonadIO m) => m ()
script2 = do
    ps <- planmillVectorAction projects
    let p = V.head ps
    p' <- planmillAction $ project $ p ^. identifier
    putPretty p
    putPretty p'
    putPretty (p == p')
    ts <- planmillVectorAction $ projectTasks $ p ^. identifier
    putPretty ts
    as <- planmillVectorAction $ projectAssignments $ p ^. identifier
    putPretty as

script3 :: (MonadPlanMill m, MonadIO m) => m ()
script3 = do
    us <- planmillVectorAction users
    let us' = V.take 5 us
    putPretty us'
    let u = V.head us
    u' <- planmillAction $ user $ u ^. identifier
    putPretty u
    putPretty u'
    putPretty (u == u')

script4 :: (MonadPlanMill m, MonadIO m) => m ()
script4 = do
    ts <- planmillVectorAction teams
    let ts' = V.take 5 ts
    putPretty ts'
    let t = V.head ts
    t' <- planmillAction $ team $ t ^. identifier
    putPretty t
    putPretty t'
    putPretty (t == t')

script5 :: (MonadPlanMill m, MonadIO m, MonadThrow m) => m ()
script5 = do
    interval <- mkResultInterval IntervalStart
        $(mkUTCTime "2015-01-01T00:00:00.000Z")
        $(mkUTCTime "2016-01-01T00:00:00.000Z")
    interval' <- mkInterval
        (utctDay $(mkUTCTime "2016-01-01T00:00:00.000Z"))
        (utctDay $(mkUTCTime "2016-02-01T00:00:00.000Z"))
    me' <- planmillAction me
    as <- planmillVectorAction $ reportableAssignments (me' ^. identifier)
    tb <- planmillAction $ userTimeBalance (me' ^. identifier)
    trs <- planmillVectorAction $ timereportsFromIntervalFor interval (me' ^. identifier)
    cc <- planmillVectorAction $ userCapacity interval' (me' ^. identifier)
    putPretty as
    putPretty tb
    putPretty trs
    putPretty cc

script6 :: (MonadPlanMill m, MonadIO m, MonadThrow m) => m ()
script6 = do
    me' <- planmillAction me
    u <- planmillAction $ user $ me' ^. identifier
    putPretty u
    c <- enumerationValue (uContractType u) (fromString "Unknown Contract")
    putPretty c
    p <- enumerationValue (uPassive u) (fromString "Unknown passivity")
    putPretty p

integration :: Bool -> Cfg -> IO ()
integration debug cfg = id
    $ evalHttpT
    $ runStderrLoggingT
    $ filterLogger logPredicate
    $ flip runReaderT cfg $ sequence_
        [ script1, script2, script3
        , script4, script5, script6
        ]
  where
    logPredicate :: LogSource -> LogLevel -> Bool
    logPredicate _ level = debug || level > LevelDebug

-------------------------------------------------------------------------------
-- Haxl example
-------------------------------------------------------------------------------

newtype H a = H { unH :: H.GenHaxl () a }

instance Functor H where
    fmap f (H x) = H (fmap f x)

instance Applicative H where
    pure = H . pure
    H f <*> H x = H (f <*> x)
    H f *> H x = H (f *> x)

instance Monad H where
    return = pure
    (>>) = (*>)
    H f >>= k = H $ f >>= unH . k

instance MonadPlanMillConstraint H where
    type MonadPlanMillC H = Unit1
    entailMonadPlanMillCVector _ _ = Sub Dict

instance MonadPlanMillQuery H where
    planmillQuery q = case (showDict, typeableDict) of
        (Dict, Dict) -> H (H.dataFetch q)
      where
        typeableDict = Q.queryDict (Proxy :: Proxy Typeable) q
        showDict     = Q.queryDict (Proxy :: Proxy Show)     q

runH :: Cfg -> H a -> IO a
runH cfg (H haxl) = do
    let stateStore = H.stateSet (initDataSourceSimpleIO LevelDebug cfg) H.stateEmpty
    env <- H.initEnv stateStore ()
    H.runHaxl env haxl

haxlScript1 :: Cfg -> IO ()
haxlScript1 cfg = do
    (us, m) <- runH cfg ((,) <$> Q.users <*> Q.me)
    putPretty m
    putPretty us
