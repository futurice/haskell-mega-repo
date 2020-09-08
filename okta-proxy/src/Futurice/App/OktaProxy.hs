{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.OktaProxy where

import Futurice.Integrations
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant
import Servant.Server.Generic

import Futurice.App.OktaProxy.API
import Futurice.App.OktaProxy.Config
import Futurice.App.OktaProxy.Ctx
import Futurice.App.OktaProxy.Logic
import Futurice.App.OktaProxy.Types

import qualified FUM.Types.Login as FUM
import qualified Personio        as P

apiServer :: Ctx -> Server OktaProxyAPI
apiServer ctx = genericServer $ Record
    { getGroupMembers = getGroupMembersImpl ctx
    , getUserApplications = getUserApplicationsImpl ctx
    }

makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
makeCtx cfg lgr mgr cache _mq = do
    let ctx = Ctx cfg lgr mgr cache

    return (ctx, [])

getGroupMembersImpl :: Ctx -> Text -> Handler [FUM.Login]
getGroupMembersImpl ctx groupName = do
    now <- currentTime
    liftIO $ runIntegrations mgr lgr now cfg $ groupMembers groupName
  where
    mgr = ctxManager ctx
    lgr = ctxLogger ctx
    cfg = cfgIntegrationsCfg (ctxConfig ctx)

getUserApplicationsImpl :: Ctx -> P.EmployeeId -> Handler (Set AppResponse)
getUserApplicationsImpl ctx employeeId = do
    now <- currentTime
    liftIO $ runIntegrations mgr lgr now cfg $ userApplications employeeId
  where
    mgr = ctxManager ctx
    lgr = ctxLogger ctx
    cfg = cfgIntegrationsCfg (ctxConfig ctx)

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService          .~ OktaProxyService
    & serverDescription      .~ "Okta proxy server"
    & serverColour           .~ (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    & serverApp oktaProxyApi .~ apiServer
    & serverEnvPfx           .~ "OKTAPROXY"
