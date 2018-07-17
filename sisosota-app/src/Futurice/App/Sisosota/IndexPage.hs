{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Sisosota.IndexPage (
    HtmlRecord, HtmlAPI, htmlApi, htmlServer
 ) where

import Futurice.Prelude
import Prelude ()
import Servant
import Servant.Multipart
import Servant.Server.Generic

import qualified Network.AWS.S3.Types as AWS

import Futurice.App.Sisosota.API
import Futurice.App.Sisosota.AWS
import Futurice.App.Sisosota.Config
import Futurice.App.Sisosota.Ctx
import Futurice.App.Sisosota.Markup
import Futurice.App.Sisosota.Types

-------------------------------------------------------------------------------
-- Server
-------------------------------------------------------------------------------

htmlServer :: Ctx -> Server HtmlAPI
htmlServer ctx = genericServer (htmlRecord ctx)

htmlRecord :: Ctx -> HtmlRecord AsServer
htmlRecord ctx = HtmlRecord
    { recIndex  = indexPageHandler []
    , recUpload = \formData -> do
        hashes <- for (files formData) $ \fd -> do
            cd <- either (\_ ->  throwError err400) pure $ mkContentData (fdPayload fd)
            liftIO $ awsUpload awsEnv bucketName
                (Just $ fdFileName fd ^. unpacked) (Just $ fdFileCType fd)
                cd
        indexPageHandler hashes
    }
  where
    cfg        = ctxConfig ctx
    bucketName = AWS.BucketName $ cfgS3Bucket cfg
    awsEnv     = ctxAwsEnv ctx

    indexPageHandler = return . indexPage

-------------------------------------------------------------------------------
-- Index page
-------------------------------------------------------------------------------

indexPage :: [ContentHash] -> HtmlPage "index"
indexPage hashes = page_ mempty (Just NavIndex) $ do
    unless (null hashes) $ do
        h2_ "Uploaded"
        ul_ $ for_ hashes $ \h ->
            li_ $ a_ [ recordHref_ recGet h ] $ toHtml h

    h2_ "Statistics"
    p_ "There aren't any yet"

    h2_ "Upload file"

    form_ [ recordAction_ recUpload, method_ "POST", enctype_ "multipart/form-data" ]$ do
        input_ [ name_ "upload-file", type_ "file" ]
        button_ [ class_ "button" ] "Submit"
