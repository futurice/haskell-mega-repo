{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.GitHubSync.Main (defaultMain) where

import Control.Applicative       (liftA3)
import Futurice.FUM.MachineAPI   (FUM6 (..), fum6)
import Futurice.Integrations
import Futurice.Lucid.Foundation (HtmlPage, fullRow_, h1_, page_)
import Futurice.Postgres         (safePoolQuery_, createPostgresPool)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant

import Futurice.App.GitHubSync.API
import Futurice.App.GitHubSync.AuditPage
import Futurice.App.GitHubSync.Config
import Futurice.App.GitHubSync.Ctx
import Futurice.App.GitHubSync.IndexPage
import Futurice.App.GitHubSync.RemoveUsers

import qualified Data.Set        as Set
import qualified FUM.Types.Login as FUM
import qualified GitHub          as GH
import qualified Personio        as P

server :: Ctx -> Server GitHubSyncAPI
server ctx = indexPageAction ctx
    :<|> auditPageAction ctx
    :<|> removeUsersAction ctx

-------------------------------------------------------------------------------
-- Auth
-------------------------------------------------------------------------------

withAuthUser
    :: (MonadIO m, MonadTime m)
    => Ctx
    -> (FUM.Login -> Integrations '[ ServFUM6, ServGH, ServPE ] (HtmlPage a))
    -> Maybe FUM.Login
    -> m (HtmlPage a)
withAuthUser ctx f = withAuthUser' page404 f ctx

page404 :: HtmlPage a
page404 = page_ "GitHub Sync - Unauthorised" $
    fullRow_ $ do
        h1_ "Unauthorised"
        "Ask IT Team for access rights"

withAuthUser'
    :: (MonadIO m, MonadTime m)
    => a
    -> (FUM.Login -> Integrations '[ ServFUM6, ServGH, ServPE ] a)
    -> Ctx -> Maybe FUM.Login
    -> m a
withAuthUser' def action ctx mfu = case mfu <|> cfgMockUser cfg of
    Nothing -> return def
    Just fu -> do
        now <- currentTime
        liftIO $ runIntegrations mgr lgr now (cfgIntegrationsCfg cfg) $ do
            fus <-fum6 $ FUMGroupEmployees $ cfgAccessGroup cfg
            if fu `Set.notMember` fus
            then return def
            else action fu
  where
    cfg = ctxConfig ctx
    mgr = ctxManager ctx
    lgr = ctxLogger ctx

withAuthUserIO
    :: (MonadIO m, MonadTime m)
    => a
    -> (FUM.Login -> IO a)
    -> Ctx -> Maybe FUM.Login
    -> m a
withAuthUserIO def action ctx mfu = case mfu <|> cfgMockUser cfg of
    Nothing -> return def
    Just fu -> do
        now <- currentTime
        liftIO $ join $ runIntegrations mgr lgr now (cfgIntegrationsCfg cfg) $ do
            fus <-fum6 $ FUMGroupEmployees $ cfgAccessGroup cfg
            if fu `Set.notMember` fus
            then return (return def)
            else return (action fu)
  where
    cfg = ctxConfig ctx
    mgr = ctxManager ctx
    lgr = ctxLogger ctx

-------------------------------------------------------------------------------
-- Endpoints
-------------------------------------------------------------------------------

indexPageAction
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "index")
indexPageAction ctx = withAuthUser ctx $ \_ -> do
    today <- currentDay
    (gh, ghi, p) <- liftA3 (,,) github githubI personioE
    pure $ indexPage today (cfgPinnedUsers cfg) gh ghi p
  where
    cfg = ctxConfig ctx

    github = toList <$> githubDetailedMembers

    githubI = do
        orgName <- view githubOrganisationName
        toList <$> githubReq (GH.orgInvitationsR orgName GH.FetchAll)
    personioE = P.personio P.PersonioEmployees

    githubDetailedMembers = do
        githubMembers <- githubOrganisationMembers
        traverse (githubReq . GH.userInfoForR . GH.simpleUserLogin) githubMembers

auditPageAction
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "audit")
auditPageAction ctx mfu = withAuthUserIO page404 impl ctx mfu where
    impl _ = runLogT "audit-page" lgr $ do
        es <- safePoolQuery_ ctx "SELECT login, timestamp, action :: text FROM \"github-sync\".auditlog ORDER BY timestamp DESC;"
        return $ auditPage es

    lgr = ctxLogger ctx

removeUsersAction
    :: Ctx
    -> Maybe FUM.Login
    -> [GH.Name GH.User]
    -> Handler (CommandResponse ())
removeUsersAction ctx mfu us = withAuthUserIO err impl ctx mfu
  where
    impl login = liftIO $ removeUsers ctx login us
    err = CommandResponseError "non authorised"


-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService           .~ GithubSyncService
    & serverDescription       .~ "Sync people from personio to github"
    & serverApp githubSyncApi .~ server
    & serverColour            .~  (Proxy :: Proxy ('FutuAccent 'AF1 'AC2))
    & serverEnvPfx            .~ "GITHUBSYNC"
  where
    makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
    makeCtx cfg lgr mgr _cache _mq = do
        pgPool <- createPostgresPool $ cfgPostgresConnInfo cfg
        let ctx = Ctx cfg lgr mgr pgPool
        pure (ctx, [])
