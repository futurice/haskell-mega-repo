{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.HoursApi (defaultMain) where

import Control.Concurrent.STM     (atomically, newTVarIO, readTVarIO, writeTVar)
import Futurice.App.Avatar.API
import Futurice.Constants         (avatarPublicUrl, avatarPublicUrlStr)
import Futurice.Integrations
import Futurice.Metrics.RateMeter (mark)
import Futurice.Periocron
import Futurice.Prelude
import Futurice.Servant
import Network.HTTP.Client        (managerConnCount)
import Prelude ()
import Servant

import Futurice.App.HoursApi.API
import Futurice.App.HoursApi.Config
import Futurice.App.HoursApi.Ctx
import Futurice.App.HoursApi.Logic
       (entryDeleteEndpoint, entryEditEndpoint, entryEndpoint, hoursEndpoint,
       projectEndpoint, userEndpoint)
import Futurice.App.HoursApi.Monad  (Hours, runHours)

import qualified Data.HashMap.Strict as HM
import qualified FUM.Types.Login     as FUM
import qualified PlanMill.Worker     as PM

server :: Ctx -> Server FutuhoursAPI
server ctx = pure "This is futuhours api"
    :<|> v1Server ctx
    :<|> debugUsers
  where
    debugUsers = liftIO $ do
        pmData <- liftIO $ readTVarIO $ ctxPersonioPlanmillMap ctx
        pure $ sort $ HM.keys pmData

v1Server :: Ctx -> Server FutuhoursV1API
v1Server ctx =
         (\mfum        -> authorisedUser ctx mfum "project" projectEndpoint)
    :<|> (\mfum        -> authorisedUser ctx mfum "user"    userEndpoint)
    :<|> (\mfum a b    -> authorisedUser ctx mfum "hours"   (hoursEndpoint a b))
    :<|> (\mfum eu     -> authorisedUser ctx mfum "entry"   (entryEndpoint eu))
    :<|> (\mfum eid eu -> authorisedUser ctx mfum "edit"    (entryEditEndpoint eid eu))
    :<|> (\mfum eid    -> authorisedUser ctx mfum "delete"  (entryDeleteEndpoint eid))

authorisedUser
    :: Ctx
    -> Maybe FUM.Login
    -> Text
    -> Hours a
    -> Handler a
authorisedUser ctx mfum meterName action =
    mcase (mfum <|> ctxMockUser ctx) (throwError err403) $ \fumUsername -> do
        let thumbSize = Just 40 -- pixels per side
        let userThumbUrl = avatarPublicUrl <> linkToText (safeLink avatarApi fumAvatarEndpoint fumUsername thumbSize False)
        pmData <- liftIO $ readTVarIO $ ctxPersonioPlanmillMap ctx
        (pemUser, pmUser) <- maybe (unauthorised fumUsername) pure $ pmData ^. at fumUsername
        liftIO $ mark $ "Request " <> meterName
        runHours ctx pmUser userThumbUrl action
  where
    unauthorised :: FUM.Login -> Handler a
    unauthorised login = do
        runLogT "auth" (ctxLogger ctx) $
            logAttention ("Unauthorised user " <> FUM.loginToText login) login
        throwError err403

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService         .~ HoursApiService
    & serverDescription     .~ "Here we mark hours"
    & serverApp futuhoursApi .~ server
    & serverColour          .~  (Proxy :: Proxy ('FutuAccent 'AF4 'AC1))
    & serverEnvPfx          .~ "FUTUHOURSAPI"
  where
    makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
    makeCtx config lgr mgr cache _mq = do
        let integrConfig = cfgIntegrationsCfg config
        let getPersonioPlanmillMap = do
                now <- currentTime
                runIntegrations mgr lgr now integrConfig personioPlanmillMap

        ppm <- getPersonioPlanmillMap
        ppmTVar <- newTVarIO ppm

        let job = mkJob "Update Planmill <- FUM map" action $ every 600
              where
                action :: IO ()
                action = do
                    m <- getPersonioPlanmillMap
                    runLogT "update-job" lgr $ logInfo "FUM users" $
                        sort $ HM.keys m
                    atomically $ writeTVar ppmTVar m

        let pmCfg = cfgPlanmillCfg config
        ws <- PM.workers lgr mgr pmCfg ["worker1", "worker2", "worker3"]

        pure $ flip (,) [job] Ctx
            { ctxPersonioPlanmillMap  = ppmTVar
            , ctxPlanmillCfg          = cfgPlanmillCfg config
            , ctxMockUser             = cfgMockUser config
            , ctxManager              = mgr
            , ctxLogger               = lgr
            , ctxCache                = cache
            , ctxIntegrationsCfg      = integrConfig
            , ctxWorkers              = ws
            }

linkToText :: Link -> Text
linkToText l = "/" <> toUrlPiece l
