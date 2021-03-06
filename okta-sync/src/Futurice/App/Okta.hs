{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Okta (defaultMain) where

import Futurice.Email            (Email)
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
import qualified Peakon          as PK
import qualified Personio        as P

apiServer :: Ctx -> Server OktaSyncAPI
apiServer ctx = genericServer $ Record
    { githubUsernames = githubUsernamesImpl ctx
    , addOktaUsers    = oktaAddUsersImpl ctx
    , startOktaSync   = startOktaSyncImpl ctx
    }

htmlServer :: Ctx -> Server HtmlAPI
htmlServer ctx = genericServer $ HtmlRecord
    { indexPageGet = indexPageImpl ctx
    }

indexPageImpl :: Ctx -> Maybe FUM.Login -> Handler (HtmlPage "indexpage")
indexPageImpl ctx login = withAuthUser ctx login $ \_ -> do
    now <- currentTime
    (es', oktaUsers, groupUsers, peakonUsers) <- liftIO $ runIntegrations mgr lgr now integrationCfg $
        (,,,) <$> P.personioEmployees
              <*> O.users
              <*> O.groupMembers internalGroup
              <*> PK.employees
    pure $ indexPage es' oktaUsers groupUsers peakonUsers
  where
    mgr = ctxManager ctx
    lgr = ctxLogger ctx
    integrationCfg = cfgIntegrationsCfg (ctxConfig ctx)

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

oktaAddUsersImpl :: Ctx -> Maybe FUM.Login -> [P.EmployeeId] -> Handler (CommandResponse ())
oktaAddUsersImpl ctx login eids = withAuthUser ctx login $ \_ -> do
    now <- currentTime
    es' <- liftIO $ runIntegrations mgr lgr now integrationCfg P.personioEmployees
    let pmap = Map.fromList $ (\p -> (p ^. P.employeeId, p)) <$> es'
    let employees = catMaybes $ fmap (flip Map.lookup pmap) eids

    void $ liftIO $ traverse (runIntegrations mgr lgr now integrationCfg . createUser) employees
    pure $ CommandResponseReload
  where
    mgr = ctxManager ctx
    lgr = ctxLogger ctx
    integrationCfg = cfgIntegrationsCfg (ctxConfig ctx)

startOktaSyncImpl :: Ctx -> Maybe FUM.Login -> Handler (CommandResponse ())
startOktaSyncImpl ctx mfum = withAuthUser ctx mfum $ \_ -> do
    _ <- liftIO $ updateJob ctx
    pure $ CommandResponseReload

groupMembers :: (MonadOkta m) => O.GroupName -> m (Set FUM.Login)
groupMembers groupName = do
    case O.groupMap ^.at groupName of
      Just group -> do
          oktaGroupMembers <- O.groupMembers $ O.giId group
          pure $ Set.fromList $ catMaybes $ (^. O.userProfile . O.profileFumUsername) <$> oktaGroupMembers
      Nothing -> pure mempty

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
            fus <- mconcat <$> traverse groupMembers (cfgAccessGroups cfg)
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
        PersonioUpdated -> do
            (OktaUpdateStats updated removed added) <- updateJob ctx
            runLogT "okta-sync" lgr $ logInfo_ $ "Updated " <> textShow (length updated) <> " employees"
            runLogT "okta-sync" lgr $ logInfo_ $ "Removed " <> textShow removed <> " employees from Peakon group"
            runLogT "okta-sync" lgr $ logInfo_ $ "Added " <> textShow added <> " employees to Peakon group"
        _ -> pure ()

    return (ctx, [])

updateJob :: Ctx -> IO OktaUpdateStats
updateJob ctx = currentTime >>= \now -> runIntegrations mgr lgr now integrationCfg $ do
    es' <- P.personioEmployees
    let es'' = filter (\p -> (p ^. P.employeeId) `Set.notMember` ignoreSet) es'
    oktaUsers <- O.users
    updateUsers ctx (utctDay now) es'' oktaUsers
  where
    cfg = ctxConfig ctx
    mgr = ctxManager ctx
    lgr = ctxLogger ctx
    integrationCfg = cfgIntegrationsCfg cfg
    ignoreSet      = cfgIgnoreFromPersonio cfg

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService          .~ OktaSyncService
    & serverDescription      .~ "Okta sync server"
    & serverColour           .~ (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    & serverHtmlApp htmlApi  .~ htmlServer
    & serverApp oktaSyncApi  .~ apiServer
    & serverEnvPfx           .~ "OKTASYNC"
