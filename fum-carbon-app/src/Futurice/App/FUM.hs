{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.FUM (defaultMain) where

import Control.Concurrent.STM    (atomically, readTVar, readTVarIO, writeTVar)
import Futurice.Lomake
import Futurice.Periocron
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant

import Futurice.App.FUM.API
import Futurice.App.FUM.Command
import Futurice.App.FUM.Config
import Futurice.App.FUM.Ctx
import Futurice.App.FUM.Markup
import Futurice.App.FUM.Pages.CreateEmployee
import Futurice.App.FUM.Pages.Index
import Futurice.App.FUM.Report.Validation
import Futurice.App.FUM.Types

import qualified Futurice.IdMap as IdMap
import qualified Personio

import qualified FUM.Types.Login as FUM

-------------------------------------------------------------------------------
-- Server
-------------------------------------------------------------------------------

apiServer :: Ctx -> Server FumCarbonMachineApi
apiServer ctx = personioRequest :<|> rawEmployees :<|> rawValidations
  where
    -- TODO: eventually move to the Logic module
    personioRequest (Personio.SomePersonioReq res) = case res of
        Personio.PersonioEmployees   -> Personio.SomePersonioRes res <$> rawEmployees
        Personio.PersonioValidations -> Personio.SomePersonioRes res <$> rawValidations
        Personio.PersonioAll         -> do
            es <- rawEmployees
            vs <- rawValidations
            pure (Personio.SomePersonioRes res (es, vs))

    rawEmployees = do
        es <- liftIO $ readTVarIO $ ctxPersonio ctx
        -- no filtering, all employees
        pure $ toList es

    rawValidations = liftIO $ readTVarIO $ ctxPersonioValidations ctx

commandServer
    :: forall path cmd. Command cmd
    => Ctx -> Server (CommandEndpoint path cmd)
commandServer ctx (LomakeRequest cmdInput) = runLogT "command" (ctxLogger ctx) $ do
    world <- liftIO $ readTVarIO (ctxWorld ctx)
    cmdInternal' <- liftIO (internalizeCommand world cmdInput)
    cmdInternal <- either undefined pure cmdInternal'
    logTrace ("command " <> commandTag (Proxy :: Proxy cmd)) cmdInternal
    case applyCommand cmdInternal world of
        Right (res, world') -> do
            -- TODO: transact
            liftIO $ atomically $ writeTVar (ctxWorld ctx) world'
            pure res
        Left err -> fail err

server :: Ctx -> Server FumCarbonApi
server ctx = indexPageImpl ctx
    :<|> createEmployeePageImpl ctx
    :<|> validationReportImpl ctx
    :<|> commandServer ctx
    :<|> apiServer ctx

-------------------------------------------------------------------------------
-- Reports
-------------------------------------------------------------------------------

validationReportImpl :: Ctx -> Handler (HtmlPage "validation-report")
validationReportImpl = liftIO . validationReport

-------------------------------------------------------------------------------
-- Endpoint wrappers
-------------------------------------------------------------------------------

indexPageImpl
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "indexpage")
indexPageImpl ctx fu = withAuthUser ctx fu impl
  where
    impl world es = pure $ indexPage world es

createEmployeePageImpl
    :: Ctx
    -> Maybe FUM.Login
    -> Personio.EmployeeId
    -> Handler (HtmlPage "create-employee")
createEmployeePageImpl ctx fu pid = withAuthUser ctx fu impl
  where
    impl world es = case es ^? ix pid of
        Just e -> pure $ createEmployeePage world es e
        Nothing -> pure notFoundPage

notFoundPage :: HtmlPage sym
notFoundPage = fumPage_ "Not found" ()
    ":("

forbiddenPage :: HtmlPage sym
forbiddenPage = fumPage_ "Forbidden" ()
    ":("

-------------------------------------------------------------------------------
-- Auth
-------------------------------------------------------------------------------

-- | Read only pages
withAuthUser
    :: (MonadIO m, MonadBase IO m, MonadTime m)
    => Ctx
    -> Maybe FUM.Login
    -> (World -> IdMap.IdMap Personio.Employee -> m (HtmlPage a))
    -> m (HtmlPage a)
withAuthUser ctx fu f = runLogT "withAuthUser" (ctxLogger ctx) $
    withAuthUser' forbiddenPage ctx fu (\w es -> lift $ f w es)

withAuthUser'
    :: (MonadIO m, MonadBase IO m, MonadTime m)
    => a                           -- ^ Response to unauthenticated users
    -> Ctx
    -> Maybe FUM.Login
    -> (World -> IdMap.IdMap Personio.Employee -> LogT m a)
    -> LogT m a
withAuthUser' def ctx fu f
    -- TODO: make proper ACL
    | fu /= Nothing = pure def
    | otherwise = do
         (world, es) <- liftIO $ atomically $ (,)
              <$> readTVar (ctxWorld ctx)
              <*> readTVar (ctxPersonio ctx)
         f world es

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName             .~ "FUM Carbon"
    & serverDescription      .~ "FUM faster than ever"
    & serverColour           .~ (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    & serverApp fumCarbonApi .~ server
    & serverEnvPfx           .~ "FUMAPP"

makeCtx :: Config -> Logger -> DynMapCache -> IO (Ctx, [Job])
makeCtx Config {..} lgr _cache = do
    mgr <- newManager tlsManagerSettings

    -- employees
    let fetchEmployees = Personio.evalPersonioReqIO mgr lgr cfgPersonioCfg Personio.PersonioAll
    (employees, validations) <- fetchEmployees

    -- context
    ctx <- newCtx
        lgr
        cfgPostgresConnInfo
        (IdMap.fromFoldable employees)
        validations
        emptyWorld

    -- jobs
    let employeesJob = mkJob "Update personio data" (updateJob ctx fetchEmployees) $ tail $ every 300

    pure (ctx, [ employeesJob ])
  where
    updateJob :: Ctx -> IO ([Personio.Employee], [Personio.EmployeeValidation]) -> IO ()
    updateJob ctx fetchEmployees = do
        (employees, validations) <- fetchEmployees
        atomically $ do
            writeTVar (ctxPersonio ctx) (IdMap.fromFoldable employees)
            writeTVar (ctxPersonioValidations ctx) validations
