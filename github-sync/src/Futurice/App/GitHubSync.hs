{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.GitHubSync (defaultMain) where

import Control.Applicative       (liftA3)
import Futurice.Integrations
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant

import Futurice.App.GitHubSync.API
import Futurice.App.GitHubSync.Config
import Futurice.App.GitHubSync.Ctx
import Futurice.App.GitHubSync.IndexPage

import qualified FUM.Types.Login as FUM
import qualified GitHub          as GH
import qualified Personio        as P

server :: Ctx -> Server GitHubSyncAPI
server ctx = indexPageAction ctx
    -- TODO: actions to add & remove

indexPageAction
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "index")
indexPageAction ctx _mfu = do
    -- TODO: restrict access to IT only
    now <- currentTime
    liftIO $ runIntegrations mgr lgr now (cfgIntegrationsConfig cfg) $ do
        today <- currentDay
        (gh, ghi, p) <- fetcher
        pure $ indexPage today (cfgPinnedUsers cfg) gh ghi p
  where
    cfg = ctxConfig ctx
    lgr = ctxLogger ctx
    mgr = ctxManager ctx

type M = Integrations '[Proxy, Proxy, Proxy, I, Proxy, I]

fetcher :: M ([GH.User], [GH.Invitation], [P.Employee])
fetcher = liftA3 (,,) github githubI personioE
  where
    github = toList <$> githubDetailedMembers
    githubI = do
        orgName <- view githubOrganisationName
        toList <$> githubReq (GH.orgInvitationsR orgName GH.FetchAll)
    personioE = P.personio P.PersonioEmployees

githubDetailedMembers :: M (Vector GH.User)
githubDetailedMembers = do
    githubMembers <- githubOrganisationMembers
    traverse (githubReq . GH.userInfoForR . GH.simpleUserLogin) githubMembers

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
        let ctx = Ctx cfg lgr mgr
        pure (ctx, [])
