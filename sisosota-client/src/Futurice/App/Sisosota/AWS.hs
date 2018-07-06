{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Sisosota.AWS where

import Futurice.Prelude
import Prelude ()
import Data.Conduit.Binary (sinkLbs)

import qualified Codec.Compression.Lzma   as LZMA
import qualified Control.Monad.Trans.AWS  as AWS
import qualified Data.HashMap.Strict      as HM
import qualified Network.AWS.S3.GetObject as AWS
import qualified Network.AWS.S3.PutObject as AWS
import qualified Network.AWS.S3.Types     as AWS

import Futurice.App.Sisosota.Types

awsUpload
    :: AWS.Env
    -> AWS.BucketName
    -> Maybe FilePath  -- ^ optional filepath
    -> Maybe Text      -- ^ optional mime type
    -> ContentData     -- ^ content data
    -> IO ContentHash
awsUpload env bucketName mfp mct (ContentData lbs) =
    AWS.runResourceT $ AWS.runAWST env $ do
        _ <- AWS.send $ AWS.putObject bucketName objectKey
            (AWS.toBody (LZMA.compress lbs))
            & AWS.poContentType     .~ mct
            & AWS.poMetadata        .~ metadata
            -- & AWS.poContentEncoding ?~ "lzma"
        return ch
  where
    ch        = contentHashLBS lbs
    objectKey = AWS.ObjectKey $ contentHashToText ch
    metadata  = HM.fromList
        [ ("original-filename" :: Text, fp ^. packed)
        | Just fp <- [ mfp ]
        ]

awsDownload
    :: AWS.Env
    -> AWS.BucketName
    -> ContentHash
    -> IO (Either String ContentData)
awsDownload env bucketName ch =
    AWS.runResourceT $ AWS.runAWST env $ do
        r <- AWS.trying AWS._ServiceError $
            AWS.send $ AWS.getObject bucketName objectKey
        case r of
            Left err -> return $ Left $ "getObject: " ++ show err
            Right r' -> do
                lbs <- AWS.sinkBody (r' ^. AWS.gorsBody) sinkLbs
                return (Right (ContentData (LZMA.decompress lbs)))
  where
    objectKey = AWS.ObjectKey $ contentHashToText ch
