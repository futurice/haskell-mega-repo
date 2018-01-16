{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Futurice.App.HoursApi.Logic (
    projectEndpoint,
    userEndpoint,
    hoursEndpoint,
    entryEndpoint,
    entryEditEndpoint,
    entryDeleteEndpoint,
    settingsEndpoint,
    updateSettingsEndpoint,
    ) where

import Control.Lens              (maximumOf, (<&>))
import FUM.Types.Login           (Login)
import Futurice.App.HoursApi.Ctx
import Futurice.Monoid           (Average (..))
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Time             (NDT (..), TimeUnit (..))
import Numeric.Interval.NonEmpty (Interval, (...))
import Prelude ()

import Futurice.App.HoursApi.Types

import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Database.PostgreSQL.Simple as Postgres
import qualified PlanMill                   as PM

-- Note: we don't import .Monad!
import qualified Futurice.App.HoursApi.Class as H

-------------------------------------------------------------------------------
-- Endpoints
-------------------------------------------------------------------------------

-- | @GET /projects@
projectEndpoint :: H.MonadHours m => m [Project ReportableTask]
projectEndpoint = reportableProjects

-- | @GET /hours@
hoursEndpoint :: H.MonadHours m => Maybe Day -> Maybe Day -> m HoursResponse
hoursEndpoint ma mb = do
    today <- currentDay
    let a = fromMaybe today ma
        b = fromMaybe today mb
        interval = a ... b
    hoursResponse interval

-- | @GET /user@
userEndpoint :: H.MonadHours m => m User
userEndpoint = userResponse


-- | Create new entry: @POST /entry@
entryEndpoint :: H.MonadHours m => EntryUpdate -> m EntryUpdateResponse
entryEndpoint eu = do
    _ <- H.task (eu ^. euTaskId) -- there should be a task
    _ <- H.addTimereport H.NewTimereport
        { H._newTimereportTaskId  = eu ^. euTaskId
        , H._newTimereportDay     = eu ^. euDate
        , H._newTimereportAmount  = eu ^. euHours
        , H._newTimereportComment = fromMaybe "" $ eu ^. euDescription
        }

    -- Building the response
    entryUpdateResponse (eu ^. euDate)


-- | @PUT /entry/#id@
entryEditEndpoint
    :: forall m. H.MonadHours m
    => PM.TimereportId -> EntryUpdate -> m EntryUpdateResponse
entryEditEndpoint eid eu = do
    tr <- H.timereport eid
    if tr ^. H.timereportTaskId == eu ^. euTaskId
    then H.editTimereport eid newTr
    else do
        H.deleteTimereport eid
        H.addTimereport newTr
    entryUpdateResponse (tr ^. H.timereportDay)
  where
    newTr = H.NewTimereport
        { H._newTimereportTaskId  = eu ^. euTaskId
        , H._newTimereportDay     = eu ^. euDate
        , H._newTimereportAmount  = eu ^. euHours
        , H._newTimereportComment = fromMaybe "" $ eu ^. euDescription
        }

-- | @DELETE /entry/#id@
entryDeleteEndpoint :: H.MonadHours m => PM.TimereportId -> m EntryUpdateResponse
entryDeleteEndpoint eid = do
    tr <- H.timereport eid
    _ <- H.deleteTimereport eid
    entryUpdateResponse (tr ^. H.timereportDay)

-- | @GET /settings@
settingsEndpoint :: Ctx -> Login -> LogT IO [SettingsResponse]
--settingsEndpoint = undefined
settingsEndpoint ctx fumUserName = safePoolQuery ctx
    "SELECT weekly_view, show_graphs FROM hours.preferences WHERE username = ?;"
    (Postgres.Only fumUserName)

--data SettingsResponse = SettingsResponse
--    { _settingsResponseWeeklyView :: !Bool
--    , _settingsResponseShowGraphs :: !Bool
--    }

-- | @POST /settings@
updateSettingsEndpoint :: Ctx -> Login -> SettingsResponse -> LogT IO SettingsUpdateResponse
updateSettingsEndpoint ctx mfum set = do
  _ <- safePoolExecute ctx "UPDATE hours.preferences SET weekly_view=?, show_graphs=? WHERE username = ?;" set
  pure SettingsUpdateResponse
    { _settingsUpdateStatus = "OK"
    , _settingsUpdateUnused = ()
    }

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

hoursResponse :: forall m. H.MonadHours m => Interval Day -> m HoursResponse
hoursResponse interval = do
    reports <- H.timereports interval

    let entries = reportToEntry <$>  toList reports

    let markedTaskIds = Map.fromListWith (<>) $
            entries <&> \e -> (e ^. entryProjectId, Set.singleton (e ^. entryTaskId))
    marked <- toList <$> itraverse markedProject markedTaskIds

    -- reportable projects; the ones we can report
    reportable <- reportableProjects

    -- holiday names
    holidayNames <- mkHolidayNames <$> H.capacities interval

    -- working hours
    wh <- H.workingHours

    pure HoursResponse
        { _hoursResponseDefaultWorkHours   = wh
        , _hoursResponseReportableProjects = reportable
        , _hoursResponseMarkedProjects     = marked
        , _hoursResponseMonths             = mkHoursMonth interval holidayNames entries
        }
  where
    mkHolidayNames :: Foldable f => f H.Capacity -> Map Day DayType
    mkHolidayNames = toMapOf (folded . getter mk . folded . ifolded)
      where
        mk :: H.Capacity -> Maybe (Day, DayType)
        mk c
            | Just desc <- c ^. H.capacityDescription, desc /= ""
                                            = mk' (DayTypeHoliday desc)
            | (c ^. H.capacityAmount) <= 0  = mk' DayTypeZero
            | otherwise                     = Nothing
          where
            mk' x = Just (c ^. H.capacityDay, x)

    reportToEntry :: H.Timereport -> Entry
    reportToEntry tr = Entry
        { _entryId          = tr ^. H.timereportId
        , _entryProjectId   = tr ^. H.timereportProjectId
        , _entryTaskId      = tr ^. H.timereportTaskId
        , _entryDay         = tr ^. H.timereportDay
        , _entryDescription = tr ^. H.timereportComment
        , _entryClosed      = tr ^. H.timereportClosed
        , _entryHours       = tr ^. H.timereportAmount
        , _entryBillable    = tr ^. H.timereportType
        }

    markedProject :: PM.ProjectId -> Set PM.TaskId -> m (Project MarkedTask)
    markedProject pid tids = do
        now <- currentTime
        p <- H.project pid
        tasks <- for (toList tids) $ \tid -> do
            t <- H.task tid
            pure MarkedTask
                { _mtaskId      = tid
                , _mtaskName    = t ^. H.taskName
                , _mtaskClosed  = now > t ^. H.taskFinish
                , _mtaskAbsence = p ^. H.projectAbsence
                }
        pure Project
            { _projectId     = pid
            , _projectName   = p ^. H.projectName
            , _projectTasks  = tasks
            , _projectClosed = p ^. H.projectClosed
            }

reportableProjects :: H.MonadHours m => m [Project ReportableTask]
reportableProjects = do
    now <- currentTime

    -- Ask Planmill for reportable assignments
    reportable <- filter (\ra -> now < ra ^. H.raFinish) <$> H.reportableAssignments

    -- Tasks per project
    let tasksPerProject :: Map PM.ProjectId (NonEmpty PM.TaskId)
        tasksPerProject = Map.fromListWith (<>) $
            reportable ^.. folded . getter reportableAcc

    projects <- for (Map.toList tasksPerProject) $ uncurry $ \pid tids -> do
        project <- H.project pid
        tasks <- for (toList tids) $ \tid -> do
            t <- H.task tid
            le <- H.latestEntry tid
            pure ReportableTask
                { _rtaskId             = t ^. H.taskId
                , _rtaskName           = t ^. H.taskName
                , _rtaskClosed         = False
                , _rtaskLatestEntry    = le
                , _rtaskHoursRemaining = Nothing
                }
        pure Project
            { _projectId     = pid
            , _projectName   = project ^. H.projectName
            , _projectTasks  = sortBy compareTasks tasks
            , _projectClosed = False
            }

    pure $ sortBy compareProjects projects
  where
    reportableAcc :: H.ReportableAssignment -> (PM.ProjectId, NonEmpty PM.TaskId)
    reportableAcc ra = (ra ^. H.raProjectId, pure $ ra ^. H.raTaskId)

    -- compare latest entries dates
    compareProjects :: Project ReportableTask -> Project ReportableTask -> Ordering
    compareProjects a b = compareMaybes (maximumOf l a) (maximumOf l b)
      where
        l = projectTasks . folded . rtaskLatestEntry . folded . latestEntryDate

    compareTasks :: ReportableTask -> ReportableTask -> Ordering
    compareTasks a b = compareMaybes (maximumOf l a) (maximumOf l b)
      where
        l = rtaskLatestEntry . folded . latestEntryDate

    -- 'Just' values are smaller. Note: reversed
    compareMaybes :: Ord a => Maybe a -> Maybe a -> Ordering
    compareMaybes (Just a) (Just b) = compare b a
    compareMaybes (Just _) Nothing  = LT
    compareMaybes Nothing  (Just _) = GT
    compareMaybes Nothing  Nothing  = EQ

userResponse :: forall m. H.MonadHours m => m User
userResponse = User
    <$> H.profileFirstName
    <*> H.profileLastName
    <*> H.flexBalance
    <*> H.vacationRemaining
    <*> utzResponse
    <*> H.profilePictureUrl
  where
    utzResponse :: m Float
    utzResponse = do
        reports <- H.timereportsLast28
        let Average _hours utz = foldMap timereportAverageUtz reports
        pure utz
      where
        timereportAverageUtz :: H.Timereport -> Average Float
        timereportAverageUtz report = case report ^. H.timereportType of
            EntryTypeBillable    -> Average hours 100
            EntryTypeNotBillable -> Average hours 0
            EntryTypeAbsence     -> mempty
          where
            NDT hours = fmap realToFrac (report ^. H.timereportAmount) :: NDT 'Hours Float

entryUpdateResponse :: H.MonadHours m => Day -> m EntryUpdateResponse
entryUpdateResponse d =
    EntryUpdateResponse <$> userResponse <*> hoursResponse (d ... d)
