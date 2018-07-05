{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Sisosota.Main (defaultMain) where

import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant
import Servant.Server.Generic

import qualified Control.Monad.Trans.AWS  as AWS
import qualified Network.AWS.Env          as AWS
import qualified Network.AWS.S3.Types     as AWS

import Futurice.App.Sisosota.API
import Futurice.App.Sisosota.Config    (Config (..))
import Futurice.App.Sisosota.Ctx
import Futurice.App.Sisosota.AWS
import Futurice.App.Sisosota.IndexPage
import Futurice.App.Sisosota.Types

-------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------

apiServer :: Ctx -> Server SisosotaAPI
apiServer ctx = genericServer $ Record
    { recGet   = getHandler ctx
    , recPut   = putHandler ctx
    }

-- | TODO: use servant-conduit with servant-0.15
getHandler :: Ctx -> ContentHash -> Handler ContentData
getHandler (Ctx cfg _ _ _ env) h = do
    x <- liftIO $ awsDownload env bucketName h
    -- let all AWS ServiceErrors be 404
    case x of
        Right x' -> return x'
        Left err -> do
            liftIO $ print err
            throwError err404
  where
    bucketName = AWS.BucketName $ cfgS3Bucket cfg

putHandler :: Ctx -> ContentData -> Handler ContentHash
putHandler (Ctx cfg _ _ _ env) d =
    liftIO $ awsUpload env bucketName Nothing Nothing d
  where
    bucketName = AWS.BucketName $ cfgS3Bucket cfg

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService          .~ SisosotaService
    & serverDescription      .~ "SisOsoTa - Content-addressable storage"
    & serverColour           .~ (Proxy :: Proxy ('FutuAccent 'AF6 'AC3))
    & serverHtmlApp htmlApi  .~ htmlServer
    & serverApp sisosotaApi  .~ apiServer
    & serverEnvPfx           .~ "SISOSOTA"
  where
    makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
    makeCtx cfg lgr mgr cache _mq = do
        env' <- AWS.newEnvWith (cfgAwsCredentials cfg) Nothing mgr
        let env = env' & AWS.envRegion .~ AWS.Frankfurt
        let ctx = Ctx cfg lgr cache mgr env
        pure (ctx, [])
