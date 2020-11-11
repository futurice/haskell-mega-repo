{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Futurice.App.Checklist (defaultMain) where

import Algebra.Graph.Class       (edges)
import Control.Applicative       (liftA3)
import Control.Concurrent.STM    (atomically, readTVarIO, writeTVar)
import Control.Lens              (re, (??))
import Data.Foldable             (foldl')
import Data.Pool                 (withResource)
import Futurice.Integrations
       (Integrations, IntegrationsConfig, MonadPersonio (..),
       githubOrganisationMembers, githubUsernamesFromOkta, personioPlanmillMap',
       runIntegrations)
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Periocron
import Futurice.Prelude
import Futurice.Servant
import Futurice.Stricter
import Prelude ()
import Servant
import Servant.Chart             (Chart)
import Servant.Graph             (Graph (..))
import Servant.Server.Generic    (genericServer)

import Futurice.App.Checklist.API
import Futurice.App.Checklist.Ack
import Futurice.App.Checklist.Charts.Done
import Futurice.App.Checklist.Command
import Futurice.App.Checklist.Config
import Futurice.App.Checklist.Ctx
import Futurice.App.Checklist.Logic
import Futurice.App.Checklist.Notifications
import Futurice.App.Checklist.Pages.AgentAudit
import Futurice.App.Checklist.Pages.Agents
import Futurice.App.Checklist.Pages.Archive
import Futurice.App.Checklist.Pages.Checklist
import Futurice.App.Checklist.Pages.Checklists
import Futurice.App.Checklist.Pages.CreateEmployee
import Futurice.App.Checklist.Pages.CreateTask
import Futurice.App.Checklist.Pages.Employee
import Futurice.App.Checklist.Pages.EmployeeAudit
import Futurice.App.Checklist.Pages.Error          (forbiddedPage, notFoundPage)
import Futurice.App.Checklist.Pages.HelpAppliance
import Futurice.App.Checklist.Pages.HelpServices
import Futurice.App.Checklist.Pages.Index
import Futurice.App.Checklist.Pages.More
import Futurice.App.Checklist.Pages.Personio
import Futurice.App.Checklist.Pages.Report
import Futurice.App.Checklist.Pages.Stats
import Futurice.App.Checklist.Pages.Task
import Futurice.App.Checklist.Pages.Tasks
import Futurice.App.Checklist.Types

import qualified Data.Map.Strict            as Map
import qualified Database.PostgreSQL.Simple as Postgres
import qualified FUM.Types.GroupName        as FUM
import qualified FUM.Types.Login            as FUM
import qualified Futurice.FUM.MachineAPI    as FUM
import qualified GitHub                     as GH
import qualified Okta                       as O
import qualified Personio
import qualified PlanMill                   as PM
import qualified PlanMill.Queries           as PMQ

-------------------------------------------------------------------------------
-- Server
-------------------------------------------------------------------------------

server :: Ctx -> Server ChecklistAPI
server ctx = genericServer $ ChecklistRoutes
    { routeIndex          = indexPageImpl ctx
    , routeTasks          = tasksPageImpl ctx
    , routeChecklists     = checklistsPageImpl ctx
    , routeCreateTask     = createTaskPageImpl ctx
    , routeCreateEmployee = createEmployeePageImpl ctx
    , routeChecklist      = checklistPageImpl ctx
    , routeChecklistGraph = checklistGraphImpl ctx
    , routeTask           = taskPageImpl ctx
    , routeEmployee       = employeePageImpl ctx
    , routeEmployeeAudit  = employeeAuditPageImpl ctx
    , routeMore           = morePageImpl ctx
    , routeAgents         = agentsPageImpl ctx
    , routeAgentAudit     = agentAuditPageImpl ctx
    , routeArchive        = archivePageImpl ctx
    , routePersonio       = personioPageImpl ctx
    , routeReport         = reportPageImpl ctx
    , routeStats          = statsPageImpl ctx
    , routeDoneChart      = doneChartImpl ctx
    , routeHelpAppliance  = applianceHelpImpl ctx
    , routeHelpServices   = servicesHelpImpl ctx
    , routeCommand        = commandImpl ctx
    }

-------------------------------------------------------------------------------
-- Endpoint wrappers
-------------------------------------------------------------------------------

indexPageImpl
    :: Ctx
    -> Maybe FUM.Login
    -> Maybe Office
    -> Maybe ChecklistId
    -> Maybe (Identifier Task)
    -> Bool
    -> Bool
    -> Handler (HtmlPage "indexpage")
indexPageImpl ctx fu loc cid tid showDone showOld = withAuthUser ctx fu impl
  where
    impl world userInfo = do
        today <- currentDay
        now <- currentTime
        integrationData <- getEmployeeExternalData now ctx
        pure $ indexPage world today userInfo integrationData loc checklist task showDone showOld
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
    -> Maybe ChecklistId
    -> Handler (HtmlPage "tasks")
tasksPageImpl ctx fu role cid = withAuthUser ctx fu impl
  where
    impl world userInfo =
        pure $ tasksPage world userInfo role checklist
      where
        checklist = do
            cid' <- cid
            world ^? worldLists . ix cid'

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
    -> Maybe ChecklistId
    -> Maybe (Identifier Employee)
    -> Maybe Personio.EmployeeId
    -> Handler (HtmlPage "create-employee")
createEmployeePageImpl ctx fu mcid meid mpeid = withAuthUser ctx fu impl
  where
    impl world userInfo = do
        let memployee = meid >>= \eid -> world ^? worldEmployees . ix eid
        pemployees <- do
            employees <- getPersonioEmployees ctx
            pure $ Map.fromList $ map (\e -> (e ^. Personio.employeeId, e)) $ employees

        pure $ createEmployeePage world userInfo mcid memployee
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
            now <- currentTime
            integrationData <- getEmployeeExternalData now ctx
            pure $ taskPage world today userInfo task integrationData

checklistPageImpl
    :: Ctx
    -> Maybe FUM.Login
    -> ChecklistId
    -> Handler (HtmlPage "checklist")
checklistPageImpl ctx fu cid = withAuthUser ctx fu impl
  where
    impl world userInfo = do
        today <- currentDay
        pure $ checklistPage world today userInfo $ world ^. worldLists . pick cid

checklistGraphImpl
    :: Ctx
    -> Maybe FUM.Login
    -> ChecklistId
    -> Handler (Graph TaskNode "checklist")
checklistGraphImpl ctx fu cid = runLogT "withAuthUser" (ctxLogger ctx) $
    withAuthUser' forbiddenGraph ctx fu impl
  where
    impl world _userInfo = pure $
        checklistGraph world $ world ^. worldLists . pick cid

    forbiddenGraph = Graph $ edges [] -- TODO

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
            integrationData <- getEmployeeExternalData now ctx
            pure (employeePage world userInfo employee integrationData)

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
    employees <- getPersonioEmployees ctx
    pure (personioPage world userInfo now employees)


reportPageImpl
    :: Ctx
    -> Maybe FUM.Login
    -> Maybe ChecklistId
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

morePageImpl
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "more")
morePageImpl ctx fu = withAuthUser ctx fu $ \world userInfo ->
    pure $ morePage world userInfo

applianceHelpImpl
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "appliance-help")
applianceHelpImpl ctx fu = withAuthUser ctx fu $ \world userInfo ->
    pure $ helpAppliancePage world userInfo

servicesHelpImpl
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "services-help")
servicesHelpImpl ctx fu = withAuthUser ctx fu $ \world userInfo ->
    pure $ helpServicesPage world userInfo

statsPageImpl
    :: Ctx
    -> Maybe FUM.Login
    -> SortCriteria
    -> Bool
    -> Bool
    -> Handler (HtmlPage "stats")
statsPageImpl ctx fu sortCriteria sortDescOrder showTaskWithoutChecklists = withAuthUser ctx fu $ \world userInfo -> do
    today <- currentDay
    pure $ statsPage world today userInfo sortCriteria sortDescOrder showTaskWithoutChecklists

-------------------------------------------------------------------------------
-- All integrations helper
-------------------------------------------------------------------------------

getEmployeeExternalData
    :: MonadIO m
    => UTCTime
    -> Ctx
    -> m IntegrationData
getEmployeeExternalData now ctx = liftIO $ do
    ps <- getPersonioEmployees ctx
    githubOkta <- liftIO $ readTVarIO $ ctxOktaGithub ctx
    cachedIO lgr cache 180 ps $ do
        (githubD, personioD, planmillD, oktaEmployees) <- runIntegrations mgr lgr now cfg $ fetchEmployeeExternalData ps
        return $ IntegrationData (githubDataMap githubD) (personioDataMap personioD) planmillD githubOkta (oktaDataMap oktaEmployees)
  where
      githubDataMap gd = Map.fromList $ map (\g -> (GH.simpleUserLogin g, g)) (toList gd)
      personioDataMap pd = Map.fromList $ map (\e -> (e ^. Personio.employeeId, e)) pd
      oktaDataMap od = Map.fromList $ map (\o -> (o ^. O.userProfile . O.profileLogin, o)) od
      lgr = ctxLogger ctx
      mgr = ctxManager ctx
      cfg = ctxIntegrationsCfg ctx
      cache = ctxCache ctx

type M = Integrations ChecklistIntegrations

personioPlanmillPMuserMap :: [Personio.Employee] -> M (HashMap FUM.Login (Personio.Employee, PMUser))
personioPlanmillPMuserMap ps = do
    persons <- personioPlanmillMap' ?? ps
    for persons $ \(per, plan) -> do
        passive <- PMQ.enumerationValue (PM.uPassive plan) "-"
        pure (per, PMUser
            { pmUser = plan
            ,pmPassive = passive
            })

fetchEmployeeExternalData :: [Personio.Employee]
    -> M ( Vector GH.SimpleUser
         , [Personio.Employee]
         , HashMap FUM.Login (Personio.Employee, PMUser)
         , [O.User])
fetchEmployeeExternalData ps = (,,,)
    <$> githubOrganisationMembers
    <*> personio Personio.PersonioEmployees
    <*> personioPlanmillPMuserMap ps
    <*> O.users

-------------------------------------------------------------------------------
-- Personio helper
-------------------------------------------------------------------------------

getPersonioEmployees :: MonadIO m => Ctx -> m [Personio.Employee]
getPersonioEmployees = liftIO . readTVarIO  . ctxPersonio

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
        case world ^? worldEmployees . ix eid <|> world ^? worldArchive . ix eid . archiveEmployee of
            Nothing -> pure notFoundPage
            Just employee -> do
                cmds <- fetchEmployeeCommands ctx employee
                pure $ employeeAuditPage world userInfo employee cmds

agentsPageImpl
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "agents")
agentsPageImpl ctx fu = withAuthUser ctx fu $ \world userInfo -> do
    agents <- fetchAgents ctx
    pure $ agentsPage world userInfo agents

agentAuditPageImpl
    :: Ctx
    -> Maybe FUM.Login
    -> FUM.Login
    -> Handler (HtmlPage "agent-audit")
agentAuditPageImpl ctx fu agent = withAuthUser ctx fu $ \world userInfo -> do
    cmds <- fetchAgentCommands ctx agent
    pure $ agentAuditPage world userInfo agent cmds

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

-------------------------------------------------------------------------------
-- Commands fetch
-------------------------------------------------------------------------------

fetchEmployeeCommands
    :: MonadBaseControl IO m
    => Ctx
    -> Employee
    -> m [(Command Identity, FUM.Login, UTCTime)]
fetchEmployeeCommands ctx e = withResource (ctxPostgres ctx) $ \conn ->
    liftBase $ Postgres.query conn query (e ^. identifier, e ^. employeeChecklist . re _ChecklistId)
  where
    query = fromString $ unwords
        [ "SELECT cmddata, username, updated FROM checklist2.commands"
        , "WHERE cmddata :: json ->> 'eid' = ? or cmddata :: json ->> 'cid' = ?"
        , "ORDER BY cid ASC"
        , ";"
        ]

fetchAgentCommands
    :: MonadBaseControl IO m
    => Ctx
    -> FUM.Login
    -> m [(Command Identity, UTCTime)]
fetchAgentCommands ctx agent = withResource (ctxPostgres ctx) $ \conn ->
    liftBase $ Postgres.query conn query (Postgres.Only agent)
  where
    query = fromString $ unwords
        [ "SELECT cmddata, updated FROM checklist2.commands"
        , "WHERE username = ?"
        , "ORDER BY cid DESC"
        , "LIMIT 200"
        , ";"
        ]

fetchAgents
    :: MonadBaseControl IO m
    => Ctx
    -> m [(FUM.Login, UTCTime)]
fetchAgents ctx = withResource (ctxPostgres ctx) $ \conn ->
    liftBase $ Postgres.query_ conn
        "select username, max(updated) from checklist2.commands group by username;"

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
    & serverService          .~ ChecklistService
    & serverDescription      .~ "Super TODO"
    & serverColour           .~ (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    & serverApp checklistApi .~ server
    & serverEnvPfx           .~ "CHECKLISTAPP"

makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
makeCtx cfg@Config {..} lgr mgr cache mq = do
    ctx <- newCtx
        lgr
        mgr
        cache
        cfgIntegrationsCfg
        cfgPostgresConnInfo
        cfgMockUser
        emptyWorld
        cfgSlackToken
        cfgSlackChannel
    cmds <- withResource (ctxPostgres ctx) $ \conn ->
        Postgres.query_ conn "SELECT username, updated, cmddata FROM checklist2.commands ORDER BY cid;"
    let world0 = foldl' (\world (fumuser, now, cmd) -> applyCommand now fumuser cmd world) emptyWorld cmds
    atomically $ writeTVar (ctxWorld ctx) world0

    -- ACL job
    let aclAction = do
            acl <- fetchGroups
                mgr
                lgr
                cfgIntegrationsCfg
                (cfgFumITGroup, cfgFumHRGroup, cfgFumSupervisorGroup)
            atomically $ writeTVar (ctxACL ctx) acl

    let aclJob = mkJob "update checklist ACL" aclAction $ every 600

    -- personio job
    let personioAction = do
          runLogT "update-from-personio" lgr $ logTrace_ "Updating"
          now <- currentTime
          ps <- runIntegrations mgr lgr now cfgIntegrationsCfg $
              personio Personio.PersonioEmployees
          atomically $ writeTVar (ctxPersonio ctx) ps

    let personioJob = mkJob "update personio data" personioAction $ every 300

    let oktaGithubAction = do
            runLogT "update-from-okta" lgr $ logTrace_ "Updating"
            now <- currentTime
            gs <- runIntegrations mgr lgr now cfgIntegrationsCfg $
                githubUsernamesFromOkta cfg
            atomically $ writeTVar (ctxOktaGithub ctx) gs

    let oktaGithubJob = mkJob "update github data from okta" oktaGithubAction $ every 300

    -- listen to MQ, especially updated Personio
    void $ forEachMessage mq $ \msg -> case msg of
        PersonioUpdated -> personioAction
        DueDatePing     -> checkDueDates ctx
        _               -> pure ()

    pure (ctx, [aclJob, personioJob, oktaGithubJob])

fetchGroups
    :: Manager
    -> Logger
    -> IntegrationsConfig ChecklistIntegrations
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
