{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Futurice.App.HoursApi.Monad (
    Hours,
    runHours,
    ) where

import Control.Lens              (Getting, filtered, firstOf, sumOf)
import Data.Aeson.Compat         (FromJSON)
import Data.Constraint
import Data.Fixed                (Centi)
import Data.List                 (maximumBy)
import Data.Ord                  (comparing)
import Data.Time                 (addDays)
import Futurice.Cache            (DynMapCache, cachedIO)
import Futurice.Constraint.Unit1 (Unit1)
import Futurice.Integrations     (IntegrationsConfig (..))
import Futurice.Prelude
import Futurice.Time             (NDT (..), ndtConvert, ndtConvert', ndtDivide)
import Numeric.Interval.NonEmpty ((...))
import Prelude ()
import Servant                   (Handler)

import Futurice.App.HoursApi.Class
import Futurice.App.HoursApi.Ctx

import qualified Futurice.App.HoursApi.Types as T
import qualified Futurice.Time               as Time
import qualified Haxl.Core                   as Haxl
import qualified PlanMill                    as PM
import qualified PlanMill.Queries            as PMQ
import qualified PlanMill.Queries.Haxl       as PMQ
import qualified PlanMill.Types.Query        as Q
import qualified PlanMill.Worker             as PM

data Env = Env
    { _envNow            :: !UTCTime
    , _envPmUser         :: !PM.User
    , _envProfilePicture :: !Text
    }

makeLenses ''Env

-- | A "real" implementation of 'MonadHours'.
--
-- /TODO:/ :)
--
newtype Hours a = Hours { _unHours :: ReaderT Env (Haxl.GenHaxl ()) a }
  deriving (Functor, Applicative, Monad)

runHours :: Ctx -> PM.User -> Text -> Hours a -> Handler a
runHours ctx pmUser profilePic (Hours m) = liftIO $ do
    -- We ask current time only once.
    now <- currentTime
    let env = Env now pmUser profilePic
    let haxl = runReaderT m env
    let stateStore
            = Haxl.stateSet (PMQ.initDataSourceBatch lgr mgr pmReq)
            . Haxl.stateSet (PlanMillDataState lgr cache ws)
            $ Haxl.stateEmpty
    haxlEnv <- Haxl.initEnv stateStore ()
    -- TODO: catch ServantErr
    Haxl.runHaxl haxlEnv haxl
  where
    lgr     = ctxLogger ctx
    mgr     = ctxManager ctx
    I pmReq = integrCfgPlanmillProxyBaseRequest (ctxIntegrationsCfg ctx)
    cache   = ctxCache ctx
    ws      = ctxWorkers ctx

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

viewHours :: Getting a Env a -> Hours a
viewHours = Hours . view

-- previewHours :: Getting (First a) Env a -> Hours (Maybe a)
-- previewHours = Hours . preview

cachedPlanmillAction :: (Typeable a, FromJSON a, NFData a, Show a) => PM.PlanMill a -> Hours a
cachedPlanmillAction = Hours . lift . Haxl.dataFetch . PlanMillRequestCached

planmillAction :: (Typeable a, FromJSON a, NFData a, Show a) => PM.PlanMill a -> Hours a
planmillAction = Hours . lift . Haxl.dataFetch . PlanMillRequest

-- | Uncached planmill write action.
--
-- Note: that the write results aren't visible via read 'planmillAction' if
-- it's performed first!
--writePlanmillAction :: (Typeable a, FromJSON a, NFData a, Show a) => PM.PlanMill a -> Hours a
--writePlanmillAction = Hours . lift . Haxl.uncachedRequest . PlanMillRequest

instance PM.MonadPlanMillConstraint Hours where
    type MonadPlanMillC Hours = Unit1
    entailMonadPlanMillCVector _ _ = Sub Dict

instance PM.MonadPlanMillQuery Hours where
    planmillQuery q = case (showDict, typeableDict) of
        (Dict, Dict) -> Hours $ lift $ Haxl.dataFetch q
      where
        typeableDict = Q.queryDict (Proxy :: Proxy Typeable) q
        showDict     = Q.queryDict (Proxy :: Proxy Show)     q

-------------------------------------------------------------------------------
-- Haxl
-------------------------------------------------------------------------------

data PlanMillRequest a where
    PlanMillRequest       :: (FromJSON a, NFData a, Show a, Typeable a) => PM.PlanMill a -> PlanMillRequest a
    PlanMillRequestCached :: (FromJSON a, NFData a, Show a, Typeable a) => PM.PlanMill a -> PlanMillRequest a

deriving instance Show (PlanMillRequest a)
deriving instance Typeable PlanMillRequest
deriving instance Eq (PlanMillRequest a)

instance Haxl.ShowP PlanMillRequest where showp = show

instance Hashable (PlanMillRequest a) where
  hashWithSalt salt (PlanMillRequest r) =
      salt `hashWithSalt` (0 :: Int) `hashWithSalt` r
  hashWithSalt salt (PlanMillRequestCached r) =
      salt `hashWithSalt` (1 :: Int) `hashWithSalt` r

instance Haxl.StateKey PlanMillRequest where
    data State PlanMillRequest = PlanMillDataState Logger DynMapCache PM.Workers

instance Haxl.DataSourceName PlanMillRequest where
  dataSourceName _ = "PlanMillRequest"

instance Haxl.DataSource u PlanMillRequest where
    fetch (PlanMillDataState lgr cache workers) _f _u blockedFetches = Haxl.SyncFetch $
        for_ blockedFetches $ \(Haxl.BlockedFetch r v) -> case r of
            PlanMillRequest pm -> PM.submitPlanMillE workers pm >>= Haxl.putResult v
            PlanMillRequestCached pm -> do
                let res' = PM.submitPlanMillE workers pm
                res <- cachedIO lgr cache 300 {- 5 minutes -} pm res'
                Haxl.putResult v res

-------------------------------------------------------------------------------
-- Instance
-------------------------------------------------------------------------------

instance MonadTime Hours where
    currentTime = Hours $ view envNow

instance MonadHours Hours where
    profilePictureUrl = Hours $ view envProfilePicture
    profileFirstName = Hours $ view (envPmUser . getter PM.uFirstName)
    profileLastName = Hours $ view (envPmUser . getter PM.uLastName)

    vacationRemaining = do
        pmUid <- viewHours (envPmUser . PM.identifier)
        -- Seems that we always need to divide by 7.5
        -- https://github.com/planmill/api/issues/12
        let wh = 7.5 :: NDT 'Time.Hours Centi
        vs <- cachedPlanmillAction (PM.userVacations pmUid)
        let holidaysLeft = sumOf (folded . getter PM.vacationDaysRemaining . getter ndtConvert') vs
        -- I wish we could do units properly.
        pure $ NDT $ ndtDivide holidaysLeft wh

    flexBalance = do
        pmUid <- viewHours (envPmUser . PM.identifier)
        ndtConvert' . view PM.tbMinutes <$>
            planmillAction (PM.userTimeBalance pmUid)

    workingHours = do
        cid <- viewHours (envPmUser . getter PM.uCalendars)
        calendars <- PMQ.capacitycalendars
        let hours = firstOf
                (folded . filtered (\c -> cid == Just (c ^. PM.identifier)) . getter PM.ccDefaultDailyWorktime . folded)
                calendars
        pure (maybe 7.7 ndtConvert' hours)

    reportableAssignments = do
        pmUid <- viewHours (envPmUser . PM.identifier)
        ras <- planmillAction (PM.reportableAssignments pmUid)
        pure (map mk $ toList ras)
      where
        mk :: PM.ReportableAssignment -> ReportableAssignment
        mk ra = ReportableAssignment
            { _raProjectId = ra ^. getter PM.raProject
            , _raTaskId    = ra ^. getter PM.raTask
            , _raFinish    = ra ^. getter PM.raTaskFinish
            }

    project pid = do
        p <- withFallback (PMQ.project pid) (PM.project pid)
        pure Project
            { _projectId     = p ^. PM.identifier
            , _projectName   = p ^. getter PM.pName
            , _projectClosed = isAbsence p
              -- TODO: closed if it's absence, but maybe be closed othersie
            , _projectAbsence = isAbsence p
            }

    task tid = do
        t <- withFallback (PMQ.task tid) (PM.task tid)
        pure Task
            { _taskId        = t ^. PM.identifier
            , _taskName      = PM.taskName t
            -- TODO: task should have project!
            -- Investigate when PM returns tasks without projectId
            , _taskProjectId = fromMaybe (PM.Ident 0) (PM.taskProject t)
            , _taskFinish    = PM.taskFinish t
            }

    latestEntry tid = do
        reports <- timereportsLast28
        pure $ case filter ((tid == ) . view timereportTaskId) reports of
            []       -> Nothing
            reports' -> Just $
                maximumBy cmp (map mk reports')
      where
        cmp = comparing (view T.latestEntryDate)

        mk :: Timereport -> T.LatestEntry
        mk tr = T.LatestEntry
            { T._latestEntryDescription = tr ^. timereportComment
            , T._latestEntryDate        = tr ^. timereportDay
            , T._latestEntryHours       = tr ^. timereportAmount
            }

    capacities interval = do
        pmUid <- viewHours (envPmUser . PM.identifier)
        map mk . toList <$> PMQ.capacities interval pmUid
      where
        mk uc = Capacity
            { _capacityDay         = PM.userCapacityDate uc
            , _capacityAmount      = ndtConvert' (PM.userCapacityAmount uc)
            , _capacityDescription = PM.userCapacityDescription uc
            }

    timereports interval = do
        pmUid <- viewHours (envPmUser . PM.identifier)
        let resultInterval = PM.ResultInterval PM.IntervalStart interval

        reports <- planmillAction (PM.timereportsFromIntervalFor resultInterval pmUid)
        traverse convertTimereport (toList reports)

    timereport pid = do
        report <- planmillAction (PM.timereport pid)
        convertTimereport report

    addTimereport tr = do
        pmUid <- viewHours (envPmUser . PM.identifier)
        void $ planmillAction $ PM.addTimereport PM.NewTimereport
            { PM.ntrTask    = tr ^. newTimereportTaskId
            , PM.ntrStart   = tr ^. newTimereportDay
            , PM.ntrAmount  = fmap truncate $ ndtConvert $ tr ^. newTimereportAmount
            , PM.ntrComment = tr ^. newTimereportComment
            , PM.ntrUser    = pmUid
            }

    -- Note: we don't do magic here. We just edit the timereport!
    editTimereport tid tr = do
        -- TODO: check that we don't edit 'taskId` ?
        pmUid <- viewHours (envPmUser . PM.identifier)
        void $ planmillAction $ PM.editTimereport PM.EditTimereport
            { PM._etrId     = tid
            , PM.etrTask    = tr ^. newTimereportTaskId
            , PM.etrStart   = tr ^. newTimereportDay
            , PM.etrAmount  = fmap truncate $ ndtConvert $ tr ^. newTimereportAmount
            , PM.etrComment = tr ^. newTimereportComment
            , PM.etrUser    = pmUid
            }

    deleteTimereport tid =
        void $ planmillAction $ PM.deleteTimereport tid

    timereportsLast28 = do
        today <- currentDay
        pmUid <- viewHours (envPmUser . PM.identifier)

        let interval = addDays (-28) today ... today
        let resultInterval = PM.ResultInterval PM.IntervalStart interval

        reports <- cachedPlanmillAction (PM.timereportsFromIntervalFor resultInterval pmUid)
        traverse convertTimereport (toList reports)

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

convertTimereport :: PM.Timereport -> Hours Timereport
convertTimereport tr = case PM.trProject tr of
    Just pid -> pure (makeTimereport pid tr)
    Nothing -> do
        t <- task (PM.trTask tr)
        pure (makeTimereport (t ^. taskProjectId) tr)

makeTimereport :: PM.ProjectId -> PM.Timereport -> Timereport
makeTimereport pid tr = Timereport
    { _timereportId        = tr ^. PM.identifier
    , _timereportTaskId    = PM.trTask tr
    , _timereportProjectId = pid
    , _timereportDay       = PM.trStart tr
    , _timereportComment   = fromMaybe "" (PM.trComment tr)
    , _timereportAmount    = ndtConvert' (PM.trAmount tr)
    , _timereportType      = billableStatus (PM.trProject tr) (PM.trBillableStatus tr)
    }

-- |
--
-- @pm-cli -- enumeration "Time report.Billable status"@
--
-- @
-- IntMap [1 : Billable
--        :3 : Non-billable
--        :4 : In billing
--        :5 : Draft invoice
--        :6 : Invoiced]
-- @
--
-- /TODO:/ we hard code enumeration values.
--
-- /TODO:/ absences should be EntryTypeAbsence
-- seems that Nothing projectId is the thing there.
--
billableStatus :: Maybe PM.ProjectId -> Int -> T.EntryType
billableStatus Nothing 3 = T.EntryTypeAbsence
billableStatus _ 3       = T.EntryTypeNotBillable
billableStatus _ 1       = T.EntryTypeBillable
billableStatus _ _       = T.EntryTypeInBilling

-- | Absences go into magic project.
isAbsence :: PM.Project -> Bool
isAbsence p = PM.pCategory p == Just 900

-- | Asks from Planmill Proxy, if it doesn't know ask from PlanMill directly.
withFallback
    :: (Typeable a, FromJSON a, NFData a, Show a)
    => Hours a -> PM.PlanMill a -> Hours a
withFallback action pm = do
    mx <- Just <$> action -- TODO: catch exceptions!
    case mx of
        Just x -> pure x
        Nothing -> cachedPlanmillAction pm
