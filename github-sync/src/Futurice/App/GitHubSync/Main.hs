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
import Futurice.Postgres         (createPostgresPool)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant

import Futurice.App.GitHubSync.API
import Futurice.App.GitHubSync.AuditPage
import Futurice.App.GitHubSync.Config
import Futurice.App.GitHubSync.Ctx
import Futurice.App.GitHubSync.IndexPage

import qualified Data.Set        as Set
import qualified FUM.Types.Login as FUM
import qualified GitHub          as GH
import qualified Personio        as P

server :: Ctx -> Server GitHubSyncAPI
server ctx = indexPageAction ctx
    :<|> auditPageAction ctx
    -- TODO: actions to add & remove

withAuthUser
    :: (MonadIO m, MonadTime m)
    => Ctx
    -> (FUM.Login -> Integrations '[Proxy, Proxy, I, I, Proxy, I] (HtmlPage a))
    -> Maybe FUM.Login
    -> m (HtmlPage a)
withAuthUser ctx f = withAuthUser' page404 f ctx

withAuthUser'
    :: (MonadIO m, MonadTime m)
    => a
    -> (FUM.Login -> Integrations '[Proxy, Proxy, I, I, Proxy, I] a)
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
auditPageAction ctx = withAuthUser ctx $ \_ -> do
    return $ auditPage []

page404 :: HtmlPage a
page404 = page_ "GitHub Sync - Unauthorised" $
    fullRow_ $ do
        h1_ "Unauthorised"
        "Ask IT Team for access rights"

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverName              .~ "GitHub Sync"
    & serverDescription       .~ "Sync people from personio to github"
    & serverApp githubSyncApi .~ server
    & serverColour            .~  (Proxy :: Proxy ('FutuAccent 'AF3 'AC1))
    & serverEnvPfx            .~ "GITHUBSYNC"
  where
    makeCtx :: Config -> Logger -> Manager -> Cache -> IO (Ctx, [Job])
    makeCtx cfg lgr mgr _cache = do
        pgPool <- createPostgresPool $ cfgPostgresConnInfo cfg
        let ctx = Ctx cfg lgr mgr pgPool
        pure (ctx, [])
