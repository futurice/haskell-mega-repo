{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Okta (defaultMain) where

import Futurice.Email            (Email)
import Futurice.FUM.MachineAPI   (FUM6 (..), fum6)
import Futurice.Integrations
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant
import Servant.Server.Generic

import Futurice.App.Okta.API
import Futurice.App.Okta.Config
import Futurice.App.Okta.Ctx
import Futurice.App.Okta.IndexPage
import Futurice.App.Okta.Logic

import qualified Data.Map        as Map
import qualified Data.Set        as Set
import qualified FUM.Types.Login as FUM
import qualified GitHub          as GH
import qualified Okta            as O
import qualified Personio        as P

apiServer :: Ctx -> Server OktaSyncAPI
apiServer ctx = genericServer $ Record
    { githubUsernames = githubUsernamesImpl ctx
    , addOktaUsers    = oktaAddUsersImpl ctx
    }

htmlServer :: Ctx -> Server HtmlAPI
htmlServer ctx = genericServer $ HtmlRecord
    { indexPageGet = indexPageImpl ctx
    }

indexPageImpl :: Ctx -> Maybe FUM.Login -> Handler (HtmlPage "indexpage")
indexPageImpl ctx login = withAuthUser ctx login $ \_ -> do
    now <- currentTime
    es' <- liftIO $ cachedIO (ctxLogger ctx) (ctxCache ctx) 180 () $ runIntegrations mgr lgr now integrationCfg P.personioEmployees
    oktaUsers <- liftIO $ cachedIO (ctxLogger ctx) (ctxCache ctx) 180 () $ runIntegrations mgr lgr now integrationCfg O.users
    pure $ indexPage es' oktaUsers
  where
    mgr = ctxManager ctx
    lgr = ctxLogger ctx
    integrationCfg = (cfgIntegrationsCfg (ctxConfig ctx))

githubUsernamesImpl :: Ctx -> Handler (Map.Map Email (Maybe (GH.Name GH.User)))
githubUsernamesImpl ctx = do
    now <- currentTime
    userMap <- liftIO $ cachedIO (ctxLogger ctx) (ctxCache ctx) 180 () $ runIntegrations mgr lgr now integrationCfg $ githubUsernamesFromOkta cfg
    pure userMap
  where
    mgr = ctxManager ctx
    lgr = ctxLogger ctx
    cfg = ctxConfig ctx
    integrationCfg = (cfgIntegrationsCfg (ctxConfig ctx))

oktaAddUsersImpl :: Ctx -> Maybe FUM.Login -> [P.Employee] -> Handler (CommandResponse ())
oktaAddUsersImpl = undefined

withAuthUser
    :: Ctx
    -> Maybe FUM.Login
    -> (FUM.Login -> Handler a)
    -> Handler a
withAuthUser ctx loc f = case loc <|> cfgMockUser cfg of
    Nothing -> throwError err403
    Just login -> do
        now <- currentTime
        notMember <- liftIO $ runIntegrations mgr lgr now (cfgIntegrationsCfg cfg) $ do
            fus <- mconcat <$> traverse (fum6 . FUMGroupEmployees) (cfgAccessGroups cfg)
            pure $ login `Set.notMember` fus
        if notMember then throwError err403 else f login
  where
    cfg = ctxConfig ctx
    mgr = ctxManager ctx
    lgr = ctxLogger ctx

makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
makeCtx cfg lgr mgr cache mq = do
    let ctx = Ctx cfg lgr mgr cache

    void $ forEachMessage mq $ \msg -> case msg of
        PersonioUpdated -> updateJob
        _ -> pure ()

    updateJob

    return (ctx, [])
  where
    integrationCfg = cfgIntegrationsCfg cfg

    updateJob = currentTime >>= \now -> runIntegrations mgr lgr now integrationCfg $ do
        es' <- P.personioEmployees
        oktaUsers <- O.users
        updateUsers es' oktaUsers

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService          .~ OktaSyncService
    & serverDescription      .~ "Okta sync server"
    & serverColour           .~ (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    & serverHtmlApp htmlApi  .~ htmlServer
    & serverApp oktaSyncApi  .~ apiServer
    & serverEnvPfx           .~ "OKTASYNC"
