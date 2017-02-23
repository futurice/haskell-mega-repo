{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import PlanMill.Internal.Prelude

import Control.Arrow        ((&&&))
import Control.Monad.Http   (HttpT, evalHttpT)
import Data.Yaml            (decodeFileEither)
import System.Environment   (getArgs)
import System.IO            (hPutStrLn, stderr)
import Data.Time            (fromGregorian, toGregorian, addDays)

import qualified Data.HashMap.Strict as HM

import Control.Monad.PlanMill
import PlanMill

import Caching     (CachingT (..))
import MonadPretty (MonadPretty (..))

data Dump = Dump
    { dumpProjects    :: HM.HashMap ProjectId Project
    , dumpTasks       :: HM.HashMap TaskId Task
    , dumpAssignments :: HM.HashMap AssignmentId Assignment
    , dumpTeams       :: HM.HashMap TeamId Team
    , dumpUsers       :: HM.HashMap UserId User
    }
    deriving Show

directUsersTeams :: (MonadPlanMill m, MonadCatch m) => m (Users, Teams)
directUsersTeams = (,) <$> u <*> t
  where
    u = planmillVectorAction users `catch` onPlanmillError (pure mempty)
    t = planmillVectorAction teams `catch` onPlanmillError (pure mempty)

getDump :: (MonadPlanMill m, MonadIO m, MonadCatch m) => m Dump
getDump = do
    me' <- planmillAction me
    liftIO $ putPretty me'
    me'' <- planmillAction $ user (me' ^. identifier)
    liftIO $ putPretty me''
    ps <- planmillVectorAction projects
    ts <- fold <$> traverse (planmillVectorAction . projectTasks . view identifier) (toList ps)
    as <- fold <$> traverse (planmillVectorAction . projectAssignments . view identifier) (toList ps)
    (us, t) <- directUsersTeams
    pure Dump
        { dumpProjects    = toLookupHM ps
        , dumpTasks       = toLookupHM ts
        , dumpAssignments = toLookupHM as
        , dumpTeams       = toLookupHM t
        , dumpUsers       = toLookupHM us
        }

onPlanmillError :: a -> PlanMillError -> a
onPlanmillError = const

toLookupHM :: (Foldable f, HasIdentifier a b) => f a -> HM.HashMap (Identifier b) a
toLookupHM = HM.fromList . map (view identifier &&& id) . toList

printDumpStats :: Dump -> IO ()
printDumpStats (Dump ps ts as t us) = do
    putStrLn $ "projects:    " <> show (HM.size ps)
    putStrLn $ "tasks:       " <> show (HM.size ts)
    putStrLn $ "assignments: " <> show (HM.size as)
    putStrLn $ "teams:       " <> show (HM.size t)
    putStrLn $ "users:       " <> show (HM.size us)

timeInterval :: (MonadIO m, MonadTime m) => Integer -> Integer -> m (Interval Day)
timeInterval x y = do
    now <- currentDay
    pure $ (addDays x now) ... (addDays y now)

-------------------------------------------------------------------------------
-- My projects
-------------------------------------------------------------------------------

myProjects :: (MonadPlanMill m, MonadIO m) => m ()
myProjects = do
    me' <- planmillAction me
    putPretty me'
    as <- planmillVectorAction $ reportableAssignments (me' ^. identifier)
    putPretty as

-------------------------------------------------------------------------------
-- Timereports
-------------------------------------------------------------------------------

myTimereports :: (MonadPlanMill m, MonadIO m, MonadTime m) => m ()
myTimereports = do
    me' <- planmillAction me
    putPretty me'
    let ident = me' ^. identifier
    u <- planmillAction $ user ident
    putPretty u
    t <- traverse (planmillAction . team) (uTeam u)
    putPretty t
    interval <- timeInterval (-7) 30
    trs <- planmillVectorAction $ timereportsFromIntervalFor
                                  (ResultInterval IntervalStart $ interval) ident
    putPretty trs

-------------------------------------------------------------------------------
-- Capacity calendar
-------------------------------------------------------------------------------

capacityCalendar :: (MonadPlanMill m, MonadIO m, MonadTime m) => m ()
capacityCalendar = do
    me' <- planmillAction me
    let ident = me' ^. identifier
    putPretty me'
    now <- currentTime
    let (year, _, _) = toGregorian $ utctDay now
    let interval = (fromGregorian year 1 1) ... (fromGregorian year 12 31)
    cc <- planmillVectorAction $ userCapacity interval $ ident
    putPretty cc

-------------------------------------------------------------------------------
-- Enumerations
-------------------------------------------------------------------------------

enumerationsCommand :: (MonadPlanMill m, MonadIO m) => m ()
enumerationsCommand = do
    me' <- planmillAction me
    putPretty me'
    u <- planmillAction $ user $ me' ^. identifier
    putPretty u
    contractType <- enumerationValue (uContractType u) "Unknown contract type"
    putPretty contractType

-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

newtype Cfg' = Cfg' { getCfg' :: Cfg }

instance FromJSON Cfg' where
    parseJSON = fmap Cfg' . parseJSON'
      where
        parseJSON' = withObject "Cfg" $ \obj ->
            Cfg <$> obj .: "userid"
                <*> (ApiKey . fromString <$> obj .: "apikey")
                <*> obj .: "baseurl"

-------------------------------------------------------------------------------
-- Cli interface
-------------------------------------------------------------------------------

main'' :: Cfg -> String -> IO ()
main'' cfg script = withStderrLogger $ \logger -> do
    action & id
        . evalHttpT
        . runLogT "planmill-cli" logger
        . flip runReaderT cfg
        . runCachingT
  where
    action :: CachingT (ReaderT Cfg (LogT (HttpT IO))) ()
    action = case script of
        "dump" -> do dump <- getDump
                     liftIO $ printDumpStats dump
        "myprojects" -> myProjects
        "capacitycalendar" -> capacityCalendar
        "timereports" -> myTimereports
        "enumerations" -> enumerationsCommand
        _      -> liftIO $ putStrLn $ "Unknown script: " ++ script

main' :: FilePath -> String -> IO ()
main' cfgPath script = do
    ecfg <- fmap getCfg' <$> decodeFileEither cfgPath
    case ecfg of
        Left err -> hPutStrLn stderr $ show err
        Right cfg -> main'' cfg script

main :: IO ()
main = do
    args <- getArgs
    case args of
        [cfgfile]         -> main' cfgfile "dump"
        [cfgfile, script] -> main' cfgfile script
        _                 ->
            hPutStrLn stderr "Usage: ./planmill-client cfgfile [script]"
