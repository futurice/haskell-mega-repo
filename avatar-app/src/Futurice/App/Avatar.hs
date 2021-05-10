{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Avatar (defaultMain) where

import Codec.Picture
       (DynamicImage (ImageRGBA8), convertRGBA8, decodeImage)
import Codec.Picture.Png         (encodeDynamicPng)
import Data.Aeson.Compat         (encode)
import Data.Conduit.Binary       (sinkLbs)
import Futurice.Integrations
import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant
import Servant.Cached
import Servant.JuicyPixels       (PNG)
import Servant.Server.Generic

import qualified Control.Monad.Trans.AWS      as AWS
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.Map.Strict              as Map
import qualified FUM
import qualified Network.AWS.Env              as AWS
import qualified Network.AWS.Lambda.Invoke    as AWS
import qualified Network.AWS.S3.GetObject     as AWS
import qualified Network.AWS.S3.ListObjectsV2 as AWS
import qualified Network.AWS.S3.Types         as AWS
import qualified Network.HTTP.Client          as HTTP

-- Avatar modules
import Futurice.App.Avatar.API
import Futurice.App.Avatar.Config
import Futurice.App.Avatar.Ctx
import Futurice.App.Avatar.Embedded
import Futurice.App.Avatar.Markup
import Futurice.App.Avatar.Types

-------------------------------------------------------------------------------
-- HTML
-------------------------------------------------------------------------------

htmlServer :: a -> Server HtmlAPI
htmlServer _ mfu = return $ page_ "Avatar" (Just NavHome) $ do
    for_ mfu $ \login -> do
        p_ $ "AVatars of  " <> toHtml login
        p_ $ do
            img_ [ src_ $ fieldLink' toUrlPiece recFum login (Just (Square 128)) False ]
            img_ [ src_ $ fieldLink' toUrlPiece recFum login (Just (Square 64)) False ]
            img_ [ src_ $ fieldLink' toUrlPiece recFum login (Just (Square 32)) False ]

-------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------

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
        return $ addHeader "public, max-age=3600" $ unsafeMkCached lbs

processOriginal :: Ctx ->  Text -> LogT IO DynamicImage'
processOriginal ctx url = do
    req <- HTTP.parseUrlThrow (url ^. unpacked)
    res <- liftIO $ HTTP.httpLbs req mgr
    let body = HTTP.responseBody res
    either fail return' $ do
        origImg <- decodeImage (BSL.toStrict body)
        encodeDynamicPng $ ImageRGBA8 $ convertRGBA8 origImg
  where
    mgr = ctxManager ctx
    return' = return . addHeader "public, max-age=3600" . unsafeMkCached

mkAvatar
    :: Ctx
    -> Text        -- ^ URL, is mandatory
    -> Maybe Size  -- ^ size, minimum size is 16
    -> Bool        -- ^ greyscale
    -> Handler DynamicImage'
mkAvatar ctx url msize grey = case fromMaybeSize msize of
    Original -> undefined
    Square size -> liftIO $ runLogT "avatar" (ctxLogger ctx) $ do
        let ap = AvatarProcess url size grey
        logTrace "fetching image" ap
        cachedAvatar ctx ap

mkFum
    :: Ctx
    -> FUM.Login
    -> Maybe Size  -- ^ size, minimum size is 16
    -> Bool        -- ^ greyscale
    -> Handler DynamicImage'
mkFum ctx@(Ctx cache lgr mgr cfg _) login msize grey = case fromMaybeSize msize of
    Original -> liftIO $ runLogT "original-fum" lgr $ do
        fumMap <- liftIO $ cachedIO lgr cache 3600 () getFumMap
        case fumMap ^? ix login of
            Nothing -> return fallback
            Just u  -> do
                case u ^. FUM.userImageUrl . lazy of
                    Nothing -> return fallback
                    Just url -> processOriginal ctx url

    Square size -> liftIO $ runLogT "avatar-fum" lgr $ do
        fumMap <- liftIO $ cachedIO lgr cache 3600 () getFumMap
        case fumMap ^? ix login of
            Nothing -> return fallback
            Just u  -> do
                case u ^. FUM.userImageUrl . lazy of
                    Nothing -> return fallback
                    Just url -> cachedAvatar ctx $
                        AvatarProcess url size grey
  where
    fallback = addHeader "public, max-age=3600" $ unsafeMkCached $ LBS.fromStrict futulogoBS

    getFumMap :: IO (Map FUM.Login FUM.User)
    getFumMap = do
        now <- currentTime
        xs <- runIntegrations mgr lgr now (cfgIntegrationCfg cfg) fumEmployeeList
        return $ Map.fromList $ map (\x -> (x ^. FUM.userName, x)) $ toList xs

server :: Ctx -> Server AvatarAPI
server ctx = genericServer $ Record
    { recGeneric = mkAvatar ctx
    , recFum     = mkFum ctx
    }

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService         .~ AvatarService
    & serverDescription     .~ "Serve smaller versions of your favourite images"
    & serverColour          .~ (Proxy :: Proxy ('FutuAccent 'AF5 'AC2))
    & serverHtmlApp htmlApi .~ htmlServer
    & serverApp avatarApi   .~ server
    & serverEnvPfx          .~ "AVATAR"
  where
    makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
    makeCtx cfg lgr mgr cache _mq = do
        env' <- AWS.newEnvWith (cfgAwsCredentials cfg) Nothing mgr
        let env = env' & AWS.envRegion .~ AWS.Frankfurt
        return (Ctx cache lgr mgr cfg env, [])
