{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Avatar (defaultMain) where

import Codec.Picture         (DynamicImage)
import Data.Aeson.Compat     (encode)
import Data.Conduit.Binary   (sinkLbs)
import Futurice.Integrations
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant
import Servant.Cached
import Servant.JuicyPixels   (PNG)

import qualified Control.Monad.Trans.AWS      as AWS
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Map.Strict              as Map
import qualified FUM
import qualified Network.AWS.Env              as AWS
import qualified Network.AWS.Lambda.Invoke    as AWS
import qualified Network.AWS.S3.GetObject     as AWS
import qualified Network.AWS.S3.ListObjectsV2 as AWS
import qualified Network.AWS.S3.Types         as AWS

-- Avatar modules
import Futurice.App.Avatar.API
import Futurice.App.Avatar.Config
import Futurice.App.Avatar.Ctx
import Futurice.App.Avatar.Embedded
import Futurice.App.Avatar.Types

type DynamicImage' = Headers '[Header "Cache-Control" Text] (Cached PNG DynamicImage)

cachedAvatar :: Ctx -> AvatarProcess -> LogT IO DynamicImage'
cachedAvatar (Ctx _ _ _ cfg env) ap =
    AWS.runResourceT $ AWS.runAWST env $ do
        -- logTrace "listObjectsV" digest
        r <- AWS.send $ AWS.listObjectsV2 bucketName
            & AWS.lovPrefix ?~ digest
        -- logTrace "listObjectV Response" (show r)
        case r ^? AWS.lovrsContents . folded of
            -- TODO: check time
            Just obj -> fetchAvatar (obj ^. AWS.oKey)
            _        -> do
                _r2 <- AWS.send $ AWS.invoke "AvatarCacheProcess" (encode ap ^. strict)
                -- logTrace "invoke Response" (show r2)
                fetchAvatar (AWS.ObjectKey digest)
  where
    digest     = avatarProcessDigest ap
    bucketName = AWS.BucketName $ cfgS3Bucket cfg

    fetchAvatar objKey = do
        r <- AWS.send $ AWS.getObject bucketName objKey
        logTrace "Response" (show r)
        lbs <- AWS.sinkBody (r ^. AWS.gorsBody) sinkLbs
        return $ addHeader "pubic, max-age=3600" $ unsafeMkCached lbs

clamp :: Ord a => a -> a -> a -> a
clamp mi ma x
    | x < mi    = mi
    | x > ma    = ma
    | otherwise = x

mkAvatar
    :: Ctx
    -> Text        -- ^ URL, is mandatory
    -> Maybe Int   -- ^ size, minimum size is 16
    -> Bool        -- ^ greyscale
    -> Handler DynamicImage'
mkAvatar ctx url msize grey = liftIO $ runLogT "avatar" (ctxLogger ctx) $ do
    let ap = AvatarProcess url (maybe 64 (clamp 16 256) msize) grey
    logTrace "fetching image" ap
    cachedAvatar ctx ap

mkFum
    :: Ctx
    -> FUM.Login
    -> Maybe Int   -- ^ size, minimum size is 16
    -> Bool        -- ^ greyscale
    -> Handler DynamicImage'
mkFum ctx@(Ctx cache lgr mgr cfg _) login msize grey = liftIO $ runLogT "avatar-fum" lgr $ do
    fumMap <- liftIO $ cachedIO lgr cache 3600 () getFumMap
    case fumMap ^? ix login of
        Nothing -> return fallback
        Just u  -> do
            case u ^. FUM.userImageUrl . lazy of
                Nothing -> return fallback
                Just url -> cachedAvatar ctx $
                    AvatarProcess url (maybe 64 (clamp 16 256) msize) grey
  where
    fallback = addHeader "pubic, max-age=3600" $ unsafeMkCached $ LBS.fromStrict futulogoBS

    getFumMap :: IO (Map FUM.Login FUM.User)
    getFumMap = do
        now <- currentTime
        xs <- runIntegrations mgr lgr now (cfgIntegrationCfg cfg) fumEmployeeList
        return $ Map.fromList $ map (\x -> (x ^. FUM.userName, x)) $ toList xs

server :: Ctx -> Server AvatarAPI
server ctx = pure "Hello from avatar app"
    :<|> mkAvatar ctx
    :<|> mkFum ctx

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService       .~ AvatarService
    & serverDescription   .~ "Serve smaller versions of your favourite images"
    & serverColour        .~ (Proxy :: Proxy ('FutuAccent 'AF5 'AC2))
    & serverApp avatarApi .~ server
    & serverEnvPfx        .~ "AVATAR"
  where
    makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
    makeCtx cfg lgr mgr cache _mq = do
        env' <- AWS.newEnvWith (cfgAwsCredentials cfg) Nothing mgr
        let env = env' & AWS.envRegion .~ AWS.Frankfurt
        return (Ctx cache lgr mgr cfg env, [])
