{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Avatar.IndexPage where

import Data.Aeson.Compat (object, (.=))
import FUM.Types.Login   (Login)
import Futurice.Prelude
import Futurice.Postgres
import Prelude ()
import Servant
import Servant.Multipart

import Futurice.App.Sisosota.Client

import qualified Data.ByteString.Lazy as LBS

-- for dev
import FUM.Types.Login (parseLogin)

import Futurice.App.Avatar.API
import Futurice.App.Avatar.Config
import Futurice.App.Avatar.Ctx
import Futurice.App.Avatar.Markup

indexPage :: Maybe Login -> HtmlPage "index"
indexPage _mfu = page_ "" (Just NavHome) $ for_ mfu $ \login -> do
    p_ $ "Avatars of  " <> toHtml login
    p_ $ do
        img_ [ recordSrc_ recFum login (Just (Square 128)) False ]
        img_ [ recordSrc_ recFum login (Just (Square 64)) False ]
        img_ [ recordSrc_ recFum login (Just (Square 32)) False ]

    h2_ "Upload new avatar"
    form_ [ recordAction_ recUpload, method_ "POST", enctype_ "multipart/form-data" ] $ do
        input_ [ name_ "upload-file", type_ "file" ]
        button_ [ class_ "button" ] "Submit"
  where
    mfu :: Maybe Login
    mfu = parseLogin "ogre"

-- TODO: check mfu
uploadPageHandler :: Ctx -> Maybe Login -> MultipartData Mem -> Handler (HtmlPage "index")
uploadPageHandler ctx _mfu formData = case mfu of
    Nothing    -> throwError err403
    Just login -> case formData ^? getter files . folded of
        Nothing -> throwError err400
        Just fd -> runLogT "upload" lgr $ do
            let lbs = fdPayload fd
            logInfoI "Upload for $login of size $size" $ object
                [ "login" .= login
                , "size"  .= LBS.length lbs
                ]
            h <- liftIO $ sisosotaPut mgr (cfgSisosotaBaseurl cfg) lbs
            logInfoI "Got hash back for $login $hash" $ object
                [ "login" .= login
                , "hash"  .= h
                ]
            _ <- safePoolExecute ctx insertQuery (login, h)
            return $ indexPage (Just login)
  where
    mfu :: Maybe Login
    mfu = parseLogin "ogre"

    mgr = ctxManager ctx
    lgr = ctxLogger ctx
    cfg = ctxConfig ctx
  
    insertQuery = fromString $ unwords
        [ "INSERT INTO avatar.employees as e (login, imagehash)"
        , "VALUES (?, ?) ON CONFLICT (login) DO UPDATE"
        , "SET imagehash = EXCLUDED.imagehash"
        , ";"
        ] 
