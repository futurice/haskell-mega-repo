{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Avatar (defaultMain) where

import Codec.Picture       (DynamicImage)
import Data.Aeson.Compat   (object, (.=))
import Futurice.Prelude
import Futurice.Servant
import Network.HTTP.Client (httpLbs, parseUrlThrow, responseBody)
import Prelude ()
import Servant
import System.IO           (hPutStrLn, stderr)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T

-- Avatar modules
import Futurice.App.Avatar.API
import Futurice.App.Avatar.Config (Config (..))
import Futurice.App.Avatar.Logic  (avatar)

type Ctx = (Logger, Cache, Manager)

type DynamicImage' = Headers '[Header "Cache-Control" Text] DynamicImage

mkAvatar
    :: Ctx
    -> Text        -- ^ URL, is mandatory
    -> Maybe Int   -- ^ size, minimum size is 16
    -> Bool        -- ^ greyscale
    -> Handler DynamicImage'
mkAvatar (lgr, cache, mgr) url msize grey = mk $ do
    logTrace "fetching image" $ object
        [ "url"    .= T.unpack url
        , "size: " .= show msize
        , "grey: " .= show grey
        ]
    req <- parseUrlThrow (T.unpack url)
    res <- liftIO $ cachedIO lgr cache 3600 url $ httpLbs req mgr
    (fmap . fmap) (addHeader "public, max-age=3600")
        . liftIO 
        . cachedIO lgr cache 3600 (url, size, grey)
        . pure
        . avatar size grey
        . responseBody
        $ res
  where
    size  = max 16 $ fromMaybe 32 msize

    mk :: LogT IO (Either String a) -> Handler a
    mk action = liftIO (runLogT "avatar" lgr action) >>= either (throwError . f) pure

    f err = ServantErr
        500
        "Avatar conversion error"
        (LBS.fromStrict . encodeUtf8 . T.pack $ err)
        []

server :: Ctx -> Server AvatarAPI
server ctx = pure "Hello from avatar app"
    :<|> mkAvatar ctx

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverName          .~ "Avatar API"
    & serverDescription   .~ "Serve smaller versions of your favourite images"
    & serverColour        .~ (Proxy :: Proxy ('FutuAccent 'AF5 'AC2))
    & serverApp avatarApi .~ server
    & serverEnvPfx        .~ "AVATAR"
  where
    makeCtx :: Config -> Logger -> Manager -> Cache -> IO (Ctx, [Job])
    makeCtx _cfg logger mgr cache = do
        return ((logger, cache, mgr), [])
