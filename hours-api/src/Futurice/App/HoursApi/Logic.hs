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
    entryDeleteMultipleEndpoint,
    ) where

import Control.Lens              (maximumOf, (<&>))
import Data.Fixed                (Centi)
import Data.List                 (nubBy)
import Futurice.Monoid           (Average (..))
import Futurice.Prelude
import Futurice.Time             (NDT (..), TimeUnit (..))
import Futurice.Time.Month
import Numeric.Interval.NonEmpty (Interval, inf, sup, (...))
import Prelude ()

import Futurice.App.HoursApi.Types

import qualified Data.List.NonEmpty                   as NE
import qualified Data.Map                             as Map
import qualified Data.Set                             as Set
import qualified Futurice.Integrations.TimereportKind as TK
import qualified Numeric.Interval.NonEmpty            as Interval
import qualified PlanMill                             as PM
import qualified PlanMill.Queries                     as PMQ

-- Note: we don't import .Monad!
import qualified Futurice.App.HoursApi.Class as H

-------------------------------------------------------------------------------
-- Endpoints
-------------------------------------------------------------------------------

-- | @GET /projects@
projectEndpoint :: H.MonadHours m => m [Project ReportableTask]
projectEndpoint = reportableProjects

-- | @GET /hours@
hoursEndpoint :: H.MonadHours m => Day -> Day -> m HoursResponse
hoursEndpoint a b = hoursResponse (a ... b)

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
    -- this should force us to get result before continuing
    _ <- if tr ^. H.timereportTaskId == eu ^. euTaskId
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

-- | POST /delete-timereports@
entryDeleteMultipleEndpoint :: H.MonadHours m => [PM.TimereportId] -> m EntryUpdateResponse
entryDeleteMultipleEndpoint eids = case NE.nonEmpty eids of
    Just eids -> do
        trs <- traverse deleteTimereport eids
        EntryUpdateResponse <$> userResponse <*> hoursResponse (firstDay trs ... lastDay trs)
    Nothing -> do
        today <- currentDay
        EntryUpdateResponse <$> userResponse <*> hoursResponse (today ... today)
  where
    deleteTimereport eid = do
        tr <- H.timereport eid
        _ <- H.deleteTimereport eid
        pure tr
    days = NE.sort . NE.map (view H.timereportDay)
    firstDay = NE.head . days
    lastDay = NE.last . days

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

hoursResponse :: forall m. H.MonadHours m => Interval Day -> m HoursResponse
hoursResponse interval = do
    -- no ApplicativeDo for now yet
    (reports, reportable, capacities, wh) <- (,,,)
        <$> H.timereports monthInterval
        <*> reportableProjects            -- reportable projects; the ones we can report
        <*> (H.capacities monthInterval)  -- holiday names
        <*> H.workingHours                -- working hours

    let holidayNames    = mkHolidayNames capacities
    let monthCapacities = mkMonthCapacities capacities

    let entries         = reportToEntry <$> toList reports

    let markedTaskIds = Map.fromListWith (<>) $
            entries <&> \e -> (e ^. entryProjectId, Set.singleton (e ^. entryTaskId))
    marked <- toList <$> itraverse markedProject markedTaskIds

    pure HoursResponse
        { _hoursResponseDefaultWorkHours   = wh
        , _hoursResponseReportableProjects = reportable
        , _hoursResponseMarkedProjects     = marked
        , _hoursResponseMonths             = mkHoursMonth monthCapacities interval holidayNames entries
        }
  where
    monthInterval' = dayToMonth (inf interval) ...  dayToMonth (sup interval)

    monthInterval =
        firstDayOfMonth (inf monthInterval')
        ...
        lastDayOfMonth (sup monthInterval')

    mkHolidayNames :: Foldable f => f H.Capacity -> Map Day DayType
    mkHolidayNames = toMapOf (folded . getter mk . folded . ifolded) where
        mk :: H.Capacity -> Maybe (Day, DayType)
        mk c
            | Just desc <- c ^. H.capacityDescription, desc /= ""
                                            = mk' (DayTypeHoliday desc)
            | (c ^. H.capacityAmount) <= 0  = mk' DayTypeZero
            | otherwise                     = Nothing
          where
            mk' x = Just (c ^. H.capacityDay, x)

    mkMonthCapacities :: Foldable f =>  f H.Capacity -> Map Month (NDT 'Hours Centi)
    mkMonthCapacities = Map.fromListWith (+) . map mk . toList where
        mk :: H.Capacity -> (Month, NDT 'Hours Centi)
        mk c = (dayToMonth $ c ^. H.capacityDay, c ^. H.capacityAmount)

    reportToEntry :: H.Timereport -> Entry
    reportToEntry tr = Entry
        { _entryId          = tr ^. H.timereportId
        , _entryProjectId   = tr ^. H.timereportProjectId
        , _entryTaskId      = tr ^. H.timereportTaskId
        , _entryDay         = tr ^. H.timereportDay
        , _entryDescription = tr ^. H.timereportComment
        , _entryClosed      = tr ^. H.timereportClosed
        , _entryHours       = tr ^. H.timereportAmount
        , _entryBillable    = tr ^. H.timereportKind . getter kindToType
        }

    markedProject :: PM.ProjectId -> Set PM.TaskId -> m (Project MarkedTask)
    markedProject pid tids = do
        now <- currentDay
        p <- H.project pid
        tasks <- for (toList tids) $ \tid -> do
            t <- H.task tid
            let isAbsence =  p ^. H.projectAbsence
            name <- if isAbsence then searchAbsenceType t (t ^. H.taskName) else return (t ^. H.taskName)
            pure MarkedTask
                { _mtaskId      = tid
                , _mtaskName    = name
                , _mtaskClosed  = now > t ^. H.taskFinish
                , _mtaskAbsence = isAbsence
                }
        pure Project
            { _projectId     = pid
            , _projectName   = p ^. H.projectName
            , _projectTasks  = nubBy ((==) `on` view mtaskId) tasks
            , _projectClosed = p ^. H.projectClosed
            }
      where
        searchAbsenceType :: H.Task -> Text -> m Text
        searchAbsenceType t def = do
            -- we ask for absences only if we need them
            absences <- H.absences interval
            -- find an absence which interval s pan includes task finish day
            return $ case filter (\ab -> Interval.member (t ^. H.taskFinish) (ab ^. H.absenceInterval)) absences of
                (a:_) -> a ^. H.absenceType
                []    -> def

reportableProjects :: H.MonadHours m => m [Project ReportableTask]
reportableProjects = do
    now <- currentDay

    -- Ask Planmill for reportable assignments
    reportable <- filter (\ra -> now <= ra ^. H.raFinish) <$> H.reportableAssignments

    -- Tasks per project
    -- we nub, because for some reason we might get same tasks multiple times.
    let tasksPerProject :: Map PM.ProjectId (NonEmpty PM.TaskId)
        tasksPerProject = Map.map NE.nub $ Map.fromListWith (<>) $
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
        timereportAverageUtz report = case report ^. H.timereportKind of
            TK.KindBillable       -> Average hours 100
            TK.KindNonBillable    -> Average hours 0
            TK.KindInternal       -> Average hours 0
            TK.KindAbsence        -> mempty
            TK.KindSickLeave      -> mempty
            TK.KindBalanceAbsence -> mempty
          where
            NDT hours = fmap realToFrac (report ^. H.timereportAmount) :: NDT 'Hours Float

entryUpdateResponse :: H.MonadHours m => Day -> m EntryUpdateResponse
entryUpdateResponse d =
    EntryUpdateResponse <$> userResponse <*> hoursResponse (d ... d)

kindToType :: TK.TimereportKind -> EntryType
kindToType TK.KindBillable       = EntryTypeBillable
kindToType TK.KindNonBillable    = EntryTypeNotBillable
kindToType TK.KindInternal       = EntryTypeNotBillable
kindToType TK.KindAbsence        = EntryTypeAbsence
kindToType TK.KindSickLeave      = EntryTypeAbsence
kindToType TK.KindBalanceAbsence = EntryTypeBalanceAbsence
