{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Futurice.App.HoursMock.Monad where

import Control.Concurrent.STM      (STM, TVar, atomically, readTVar, writeTVar)
import Control.Lens                (filtered)
import Data.List                   (find)
import Data.Time                   (toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Futurice.Prelude
import Numeric.Interval.NonEmpty   (inf, member, sup)
import Prelude ()
import Servant                     (Handler)

import Futurice.App.HoursApi.Class
import Futurice.App.HoursApi.Types     (EntryType (..))
import Futurice.App.HoursMock.Ctx
import Futurice.App.HoursMock.MockData
import Futurice.App.HoursMock.World

import qualified PlanMill as PM

data Env = Env
    { _envNow   :: !UTCTime
    , _envWorld :: !(TVar World)
    }

makeLenses ''Env

newtype Hours a = Hours { _unHours :: ReaderT Env STM a }
  deriving (Functor, Applicative, Monad)

runHours :: Ctx -> Hours a -> Handler a
runHours (Ctx worldTVar) (Hours m) = do
    now <- currentTime
    liftIO $ atomically $
        runReaderT m (Env now worldTVar)

-------------------------------------------------------------------------------
-- Instance
-------------------------------------------------------------------------------

instance MonadTime Hours where
    currentTime = Hours $ view envNow

readonly :: (World -> a) -> Hours a
readonly f = Hours $ do
    worldTVar <- view envWorld
    world <- lift (readTVar worldTVar)
    return (f world)

modifyTimereports :: ([Timereport] -> [Timereport]) -> Hours ()
modifyTimereports f = Hours $ do
    worldTVar <- view envWorld
    world <- lift (readTVar worldTVar)
    let world' = over worldTimereports f world
    lift (writeTVar worldTVar world')

instance MonadHours Hours where
    -- Hard coded values
    vacationRemaining = pure 15
    profilePictureUrl = pure "https://avatars1.githubusercontent.com/u/852157?v = 4&s = 200"
    profileFirstName  = pure "Test"
    profileLastName   = pure "Acco"
    workingHours      = pure 7.5

    -- semi hard values
    flexBalance       = pure 42

    capacities interval = pure (map mkCapacity [inf interval .. sup interval])
      where
        mkCapacity :: Day -> Capacity
        mkCapacity d
            -- every 5th of month is bank holiday :)
            | md == 5 = Capacity
                { _capacityDay         = d
                , _capacityAmount      = 0
                , _capacityDescription = Just "Bank"
                }
            | otherwise = Capacity
                { _capacityDay         = d
                , _capacityAmount      = if wd `elem` [6,7] then 0 else 7.5
                , _capacityDescription = Nothing
                }
          where
            (_, _, md) = toGregorian d
            (_, _, wd) = toWeekDate d

    task tid = maybe (fail $ "invalid task id " ++ show tid) pure $
        find ((tid ==) . view taskId) allTasks

    project pid = maybe (fail $ "invalid project id " ++ show pid) pure $
        find ((pid ==) . view projectId) allProjects

    reportableAssignments = pure
        [ mk taskDevelopment
        , mk taskDesign
        , mk taskInternal
        ]
      where
        mk t = ReportableAssignment
            { _raProjectId = t ^. taskProjectId
            , _raTaskId    = t ^. taskId
            , _raFinish    = t ^. taskFinish
            }

    -- timereports

    timereport tid = readonly $ \world ->
        fromMaybe (error "inconsistency") $
            world ^? worldTimereports . folded . filtered p
      where
        p tr = tr ^. timereportId == tid

    timereports interval = readonly $ \world ->
        world ^.. worldTimereports . folded . filtered p
      where
        p tr = member (tr ^. timereportDay) interval

    addTimereport ntr = modifyTimereports f where
        f reports = report : reports where
            report = Timereport
                { _timereportId        = PM.Ident (1 + maxId)
                , _timereportTaskId    = tid
                , _timereportProjectId = pid
                , _timereportDay       = ntr ^. newTimereportDay
                , _timereportComment   = ntr ^. newTimereportComment
                , _timereportAmount    = ntr ^. newTimereportAmount
                , _timereportType      = EntryTypeBillable -- :)
                , _timereportClosed    = False
                }
            maxId = case reports of
                [] -> 0
                _  -> maximum (map ((\(PM.Ident i) -> i). view timereportId) reports)
            tid = ntr ^. newTimereportTaskId
            pid = maybe (error "inconsistency") (view taskProjectId) $
                find ((tid ==) . view taskId) allTasks

    deleteTimereport tid = modifyTimereports f where
        f = filter ((tid /=) . view timereportId)

    -- "lazy" approach, cause change of timereportId
    editTimereport tid ntr = do
        deleteTimereport tid
        addTimereport ntr

    absences interval = pure []
