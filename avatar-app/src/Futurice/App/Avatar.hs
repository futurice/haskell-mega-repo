{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Avatar (defaultMain) where

import Codec.Picture           (DynamicImage)
import Control.Concurrent      (getNumCapabilities)
import Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import Control.Exception       (bracket_)
import Data.Aeson.Compat       (object, (.=))
import Futurice.Integrations
import Futurice.Prelude
import Futurice.Servant
import Network.HTTP.Client     (httpLbs, parseUrlThrow, responseBody)
import Prelude ()
import Servant
import Servant.Cached
import Servant.JuicyPixels     (PNG)

import qualified Crypto.Hash.SHA512   as SHA512
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict      as Map
import qualified Data.Text            as T
import qualified FUM

-- Avatar modules
import Futurice.App.Avatar.API
import Futurice.App.Avatar.Config
import Futurice.App.Avatar.Ctx
import Futurice.App.Avatar.Embedded
import Futurice.App.Avatar.Logic    (avatar)

type DynamicImage' = Headers '[Header "Cache-Control" Text] (Cached PNG DynamicImage)

cachedAvatar :: Ctx -> Maybe Int -> Bool -> BS.ByteString -> Handler DynamicImage'
cachedAvatar (Ctx cache lgr _ _ sem) msize grey bs = withSem sem $ mk
    . (fmap . fmap) (addHeader "public, max-age=3600")
    . liftIO
    . cachedIO lgr cache 3600 (digest, size, grey)
    . pure
    . fmap mkCached
    . avatar size grey
    $ bs
  where
    digest = SHA512.hash bs

    size  = max 16 $ fromMaybe 32 msize

    mk :: LogT IO (Either String a) -> Handler a
    mk action = liftIO (runLogT "avatar" lgr action) >>= either (throwError . f) pure

    f err = ServantErr
        500
        "Avatar conversion error"
        (LBS.fromStrict . encodeUtf8 . T.pack $ err)
        []

mkAvatar
    :: Ctx
    -> Text        -- ^ URL, is mandatory
    -> Maybe Int   -- ^ size, minimum size is 16
    -> Bool        -- ^ greyscale
    -> Handler DynamicImage'
mkAvatar ctx@(Ctx _cache lgr mgr _ _) url msize grey = runLogT "avatar" lgr $ do
    logTrace "fetching image" $ object
        [ "url"    .= T.unpack url
        , "size: " .= show msize
        , "grey: " .= show grey
        ]
    req <- parseUrlThrow (T.unpack url)
    res <- liftIO $ httpLbs req mgr
    lift $ cachedAvatar ctx msize grey $ LBS.toStrict $ responseBody res

mkFum
    :: Ctx
    -> FUM.Login
    -> Maybe Int   -- ^ size, minimum size is 16
    -> Bool        -- ^ greyscale
    -> Handler DynamicImage'
mkFum ctx@(Ctx cache lgr mgr cfg sem) login msize grey = withSem sem $ do
    fumMap <- liftIO $ cachedIO lgr cache 3600 () getFumMap
    bs <- case fumMap ^? ix login of
        Nothing -> return futulogoBS
        Just u  -> do
            case u ^. FUM.userImageUrl . lazy of
                Nothing -> return futulogoBS
                Just url -> runLogT "avatar-fum" lgr $ do
                    logTrace "fetching FUM image" $ object
                        [ "login"  .= login
                        , "url"    .= T.unpack url
                        , "size: " .= show msize
                        , "grey: " .= show grey
                        ]
                    req <- parseUrlThrow (T.unpack url)
                    res <- liftIO $ httpLbs req mgr
                    return $ LBS.toStrict $ responseBody res
    cachedAvatar ctx msize grey bs
  where
    getFumMap :: IO (Map FUM.Login FUM.User)
    getFumMap = do
        now <- currentTime
        xs <- runIntegrations mgr lgr now cfg fumEmployeeList
        return $ Map.fromList $ map (\x -> (x ^. FUM.userName, x)) $ toList xs

withSem :: QSem -> Handler a -> Handler a
withSem qsem action
    = mkHandler
    $ bracket_ (waitQSem qsem) (signalQSem qsem)
    $ runHandler action
  where
    mkHandler = Handler . ExceptT

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
        n <- getNumCapabilities
        qsem <- newQSem (max 2 n)
        return (Ctx cache lgr mgr cfg qsem, [])
