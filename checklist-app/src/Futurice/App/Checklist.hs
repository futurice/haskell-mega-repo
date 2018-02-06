{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Futurice.App.Checklist (defaultMain) where

import Control.Applicative       (liftA3)
import Control.Concurrent.STM    (atomically, readTVarIO, writeTVar)
import Data.Foldable             (foldl')
import Data.Pool                 (withResource)
import Futurice.Integrations
       (IntegrationsConfig, MonadPersonio (..), runIntegrations)
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Periocron
import Futurice.Prelude
import Futurice.Servant
import Futurice.Stricter
import Prelude ()
import Servant
import Servant.Chart             (Chart)

import Futurice.App.Checklist.Ack
import Futurice.App.Checklist.API
import Futurice.App.Checklist.Charts.Done
import Futurice.App.Checklist.Command
import Futurice.App.Checklist.Config
import Futurice.App.Checklist.Ctx
import Futurice.App.Checklist.Logic
import Futurice.App.Checklist.Pages.Archive
import Futurice.App.Checklist.Pages.Checklist
import Futurice.App.Checklist.Pages.Checklists
import Futurice.App.Checklist.Pages.CreateChecklist
import Futurice.App.Checklist.Pages.CreateEmployee
import Futurice.App.Checklist.Pages.CreateTask
import Futurice.App.Checklist.Pages.Employee
import Futurice.App.Checklist.Pages.EmployeeAudit
import Futurice.App.Checklist.Pages.Error
       (forbiddedPage, notFoundPage)
import Futurice.App.Checklist.Pages.HelpAppliance
import Futurice.App.Checklist.Pages.Index
import Futurice.App.Checklist.Pages.Personio
import Futurice.App.Checklist.Pages.Report
import Futurice.App.Checklist.Pages.Task
import Futurice.App.Checklist.Pages.Tasks
import Futurice.App.Checklist.Types

import qualified Data.Map.Strict            as Map
import qualified Database.PostgreSQL.Simple as Postgres
import qualified FUM.Types.GroupName        as FUM
import qualified FUM.Types.Login            as FUM
import qualified Futurice.FUM.MachineAPI    as FUM

import qualified Personio

-------------------------------------------------------------------------------
-- Server
-------------------------------------------------------------------------------

server :: Ctx -> Server ChecklistAPI
server ctx = indexPageImpl ctx
    :<|> tasksPageImpl ctx
    :<|> checklistsPageImpl ctx
    :<|> createChecklistPageImpl ctx
    :<|> createTaskPageImpl ctx
    :<|> createEmployeePageImpl ctx
    :<|> checklistPageImpl ctx
    :<|> taskPageImpl ctx
    :<|> employeePageImpl ctx
    :<|> employeeAuditPageImpl ctx
    :<|> archivePageImpl ctx
    :<|> personioPageImpl ctx
    :<|> reportPageImpl ctx
    :<|> doneChartImpl ctx
    :<|> applianceHelpImpl ctx
    :<|> commandImpl ctx

-------------------------------------------------------------------------------
-- Endpoint wrappers
-------------------------------------------------------------------------------

indexPageImpl
    :: Ctx
    -> Maybe FUM.Login
    -> Maybe Office
    -> Maybe (Identifier Checklist)
    -> Maybe (Identifier Task)
    -> Bool
    -> Bool
    -> Handler (HtmlPage "indexpage")
indexPageImpl ctx fu loc cid tid showDone showOld = withAuthUser ctx fu impl
  where
    impl world userInfo = do
        today <- currentDay
        pure $ indexPage world today userInfo loc checklist task showDone showOld
      where
        checklist = do
            cid' <- cid
            world ^? worldLists . ix cid'

        task = do
            tid' <- tid
            world ^? worldTasks . ix tid'

tasksPageImpl
    :: Ctx
    -> Maybe FUM.Login
    -> Maybe TaskRole
    -> Maybe (Identifier Checklist)
    -> Handler (HtmlPage "tasks")
tasksPageImpl ctx fu role cid = withAuthUser ctx fu impl
  where
    impl world userInfo =
        pure $ tasksPage world userInfo role checklist
      where
        checklist = do
            cid' <- cid
            world ^? worldLists . ix cid'

createChecklistPageImpl
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "create-checklist")
createChecklistPageImpl ctx fu = withAuthUser ctx fu impl
  where
    impl world userInfo = pure $ createChecklistPage world userInfo

createTaskPageImpl
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "create-task")
createTaskPageImpl ctx fu = withAuthUser ctx fu impl
  where
    impl world userInfo = pure $ createTaskPage world userInfo

createEmployeePageImpl
    :: Ctx
    -> Maybe FUM.Login
    -> Maybe (Identifier Employee)
    -> Maybe Personio.EmployeeId
    -> Handler (HtmlPage "create-employee")
createEmployeePageImpl ctx fu meid mpeid = withAuthUser ctx fu impl
  where
    impl world userInfo = do
        let memployee = meid >>= \eid -> world ^? worldEmployees . ix eid
        now <- currentTime
        pemployees <- do
            employees <- getPersonioEmployees now ctx
            pure $ Map.fromList $ map (\e -> (e ^. Personio.employeeId, e)) $ employees

        pure $ createEmployeePage world userInfo memployee
            (mpeid >>= \eid -> pemployees ^? ix eid)
            pemployees

checklistsPageImpl
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "checklists")
checklistsPageImpl ctx fu = withAuthUser ctx fu impl
  where
    impl world userInfo = pure $ checklistsPage world userInfo

taskPageImpl
    :: Ctx
    -> Maybe FUM.Login
    -> Identifier Task
    -> Handler (HtmlPage "task")
taskPageImpl ctx fu tid = withAuthUser ctx fu impl
  where
    impl world userInfo = case world ^? worldTasks . ix tid of
        Nothing   -> pure notFoundPage
        Just task -> do
            today <- currentDay
            pure $ taskPage world today userInfo task

checklistPageImpl
    :: Ctx
    -> Maybe FUM.Login
    -> Identifier Checklist
    -> Handler (HtmlPage "checklist")
checklistPageImpl ctx fu cid = withAuthUser ctx fu impl
  where
    impl world userInfo = case world ^? worldLists . ix cid of
        Nothing        -> pure notFoundPage
        Just checklist -> do
            today <- currentDay
            pure $ checklistPage world today userInfo checklist

employeePageImpl
    :: Ctx
    -> Maybe FUM.Login
    -> Identifier Employee
    -> Handler (HtmlPage "employee")
employeePageImpl ctx fu eid = withAuthUser ctx fu impl
  where
    impl world userInfo = case world ^? worldEmployees . ix eid of
        Nothing       -> pure notFoundPage
        Just employee -> do
            now <- currentTime
            pemployees <- do
                employees <- getPersonioEmployees now ctx
                pure $ Map.fromList $ map (\e -> (e ^. Personio.employeeId, e)) $ employees
            pure (employeePage world userInfo employee pemployees)

archivePageImpl
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "archive")
archivePageImpl ctx fu = withAuthUser ctx fu $ \world userInfo ->
    pure $ archivePage world userInfo

personioPageImpl
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "personio")
personioPageImpl ctx fu = withAuthUser ctx fu $ \world userInfo -> do
    now <- currentTime
    employees <- getPersonioEmployees now ctx
    pure (personioPage world userInfo now employees)


reportPageImpl
    :: Ctx
    -> Maybe FUM.Login
    -> Maybe (Identifier Checklist)
    -> Maybe Day
    -> Maybe Day
    -> Handler (HtmlPage "report")
reportPageImpl ctx fu cid fday tday= withAuthUser ctx fu $ \world userInfo ->
    pure $ reportPage world userInfo cid fday tday

doneChartImpl
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (Chart "done")
doneChartImpl ctx fu = withAuthUserChart ctx fu $ \world userInfo -> do
    today <- currentDay
    pure $ doneChart world today userInfo

applianceHelpImpl
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "appliance-help")
applianceHelpImpl ctx fu = withAuthUser ctx fu $ \world userInfo ->
    pure $ helpAppliancePage world userInfo

-------------------------------------------------------------------------------
-- Personio helper
-------------------------------------------------------------------------------

getPersonioEmployees :: MonadIO m => UTCTime -> Ctx -> m [Personio.Employee]
getPersonioEmployees now ctx = liftIO $ cachedIO lgr cache 180 () $
    runIntegrations mgr lgr now cfg $ personio Personio.PersonioEmployees
  where
    lgr = ctxLogger ctx
    mgr = ctxManager ctx
    cfg = ctxIntegrationsCfg ctx
    cache = ctxCache ctx

-------------------------------------------------------------------------------
-- Audit
-------------------------------------------------------------------------------

employeeAuditPageImpl
    :: Ctx
    -> Maybe FUM.Login
    -> Identifier Employee
    -> Handler (HtmlPage "employee-audit")
employeeAuditPageImpl ctx fu eid = withAuthUser ctx fu impl
  where
    impl world userInfo =
        case world ^? worldEmployees . ix eid <|> world ^? worldArchive . ix eid . _1 of
            Nothing -> pure notFoundPage
            Just employee -> do
                cmds <- fetchEmployeeCommands ctx employee
                pure $ employeeAuditPage world userInfo employee cmds

-------------------------------------------------------------------------------
-- Command implementation
-------------------------------------------------------------------------------

commandImpl
    :: (MonadIO m, MonadBaseControl IO m, MonadTime m)
    => Ctx
    -> Maybe FUM.Login
    -> Command Proxy
    -> m Ack
commandImpl ctx fu cmd = runLogT "command" (ctxLogger ctx) $
    withAuthUser' (AckErr "forbidden") ctx fu $ \_world (fumUsername, _) -> do
        (cmd', res) <- instantiatedCmd
        now <- currentTime
        ctxApplyCmd now fumUsername cmd' ctx
        pure res
  where
    instantiatedCmd = flip runStricterT mempty $ traverseCommand genIdentifier cmd

    genIdentifier
        :: (MonadIO m, MonadWriter Ack m)
        => CIT x -> Proxy (Identifier x) -> m (Identity (Identifier x))
    genIdentifier CITEmployee Proxy = do
        eid <- Identifier <$> ctxGetCRandom ctx
        tell $ AckLoad $ toUrlPiece $
            safeLink checklistApi employeePageEndpoint eid
        pure (Identity eid)
    genIdentifier CITTask Proxy = do
        tid <- Identifier <$> ctxGetCRandom ctx
        tell $ AckLoad $ toUrlPiece $
            safeLink checklistApi taskPageEndpoint tid
        pure (Identity tid)
    genIdentifier CITChecklist Proxy = do
        cid <- Identifier <$> ctxGetCRandom ctx
        tell $ AckLoad $ toUrlPiece $
            safeLink checklistApi checklistPageEndpoint cid
        pure (Identity cid)

-------------------------------------------------------------------------------
-- Commands fetch
-------------------------------------------------------------------------------

fetchEmployeeCommands
    :: MonadBaseControl IO m
    => Ctx
    -> Employee
    -> m [(Command Identity, FUM.Login, UTCTime)]
fetchEmployeeCommands ctx e = withResource (ctxPostgres ctx) $ \conn ->
    liftBase $ Postgres.query conn query (e ^. identifier, e ^. employeeChecklist)
  where
    query = fromString $ unwords
        [ "SELECT cmddata, username, updated FROM checklist2.commands"
        , "WHERE cmddata :: json ->> 'eid' = ? or cmddata :: json ->> 'cid' = ?"
        , "ORDER BY cid ASC"
        , ";"
        ]

-------------------------------------------------------------------------------
-- Auth
-------------------------------------------------------------------------------

-- | Read only pages
withAuthUser
    :: (MonadIO m, MonadBase IO m, MonadTime m)
    => Ctx -> Maybe FUM.Login
    -> (World -> AuthUser -> m (HtmlPage a))
    -> m (HtmlPage a)
withAuthUser ctx fu f = runLogT "withAuthUser" (ctxLogger ctx) $
    withAuthUser' forbiddedPage ctx fu (\w u -> lift $ f w u)

withAuthUserChart
    :: (MonadIO m, MonadBase IO m, MonadTime m)
    => Ctx -> Maybe FUM.Login
    -> (World -> AuthUser -> m (Chart a))
    -> m (Chart a)
withAuthUserChart ctx fu f = runLogT "withAuthUser" (ctxLogger ctx) $
    withAuthUser' (error "404 chart") ctx fu (\w u -> lift $ f w u)

withAuthUser'
    :: (MonadIO m, MonadBase IO m, MonadTime m)
    => a                           -- ^ Response to unauthenticated users
    -> Ctx
    -> Maybe FUM.Login
    -> (World -> AuthUser -> LogT m a)
    -> LogT m a
withAuthUser' def ctx fu f = do
    acl <- liftIO $ readTVarIO $ ctxACL ctx
    let fu'      = fu <|> ctxMockUser ctx
        authUser = fu' >>= \fu'' -> (fu'',) <$> acl ^. at fu''
    case authUser of
        Nothing -> do
            logInfo_ $ "Unauthorised user " <> textShow fu
            pure def
        Just authUser' -> do
            world <- liftIO $ readTVarIO (ctxWorld ctx)
            f world authUser'

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverName             .~ "Checklist"
    & serverDescription      .~ "Super TODO"
    & serverColour           .~ (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    & serverApp checklistApi .~ server
    & serverEnvPfx           .~ "CHECKLISTAPP"

makeCtx :: Config -> Logger -> Manager -> Cache -> IO (Ctx, [Job])
makeCtx Config {..} lgr mgr cache = do
    ctx <- newCtx
        lgr
        mgr
        cache
        cfgIntegrationsCfg
        cfgPostgresConnInfo
        cfgMockUser
        emptyWorld
    cmds <- withResource (ctxPostgres ctx) $ \conn ->
        Postgres.query_ conn "SELECT username, updated, cmddata FROM checklist2.commands ORDER BY cid;"
    let world0 = foldl' (\world (fumuser, now, cmd) -> applyCommand now fumuser cmd world) emptyWorld cmds
    atomically $ writeTVar (ctxWorld ctx) world0

    -- We don't need manager in ctx for now
    let action = do
            acl <- fetchGroups
                (ctxManager ctx)
                lgr
                cfgIntegrationsCfg
                (cfgFumITGroup, cfgFumHRGroup, cfgFumSupervisorGroup)
            atomically (writeTVar (ctxACL ctx) acl)

    let job = mkJob "update checklist ACL" action $ every 600

    pure (ctx, [job])

fetchGroups
    :: Manager
    -> Logger
    -> IntegrationsConfig '[Proxy, Proxy, I, Proxy, Proxy, I]
    -> (FUM.GroupName, FUM.GroupName, FUM.GroupName)
    -> IO (Map FUM.Login TaskRole)
fetchGroups mgr lgr cfg (itGroupName, hrGroupName, supervisorGroupName) = do
    now <- currentTime
    (itGroup, hrGroup, supervisorGroup) <- runIntegrations mgr lgr now cfg $
        liftA3 (,,)
            (FUM.fum6 $ FUM.FUMGroupEmployees itGroupName)
            (FUM.fum6 $ FUM.FUMGroupEmployees hrGroupName)
            (FUM.fum6 $ FUM.FUMGroupEmployees supervisorGroupName)
    pure $ toMapOf (folded . ifolded) $
        [ (login, TaskRoleIT) | login <- itGroup ^.. folded ] ++
        [ (login, TaskRoleHR) | login <- hrGroup ^.. folded ] ++
        [ (login, TaskRoleSupervisor) | login <- supervisorGroup ^.. folded ]
