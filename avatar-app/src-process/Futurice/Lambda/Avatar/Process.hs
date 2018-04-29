{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.Lambda.Avatar.Process (
    avatarProcessLambda,
    ) where

import Codec.Picture
       (DynamicImage (..), Image (..), PixelRGBA8, convertRGBA8, decodeImage)
import Codec.Picture.Png      (encodeDynamicPng)
import Codec.Picture.ScaleDCT (scale)
import Codec.Picture.Types    (extractLumaPlane)
import Control.Monad          (forM_)
import Foreign.Marshal.Utils  (copyBytes)
import Foreign.Ptr            (plusPtr)
import Futurice.Aeson         (object, (.=))
import Futurice.EnvConfig
import Futurice.Lambda
import Futurice.Prelude
import Prelude ()
import System.IO.Unsafe       (unsafePerformIO)

import qualified Control.Monad.Trans.AWS      as AWS
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV
import qualified Network.AWS.Env              as AWS
import qualified Network.AWS.S3.PutObject     as AWS
import qualified Network.AWS.S3.Types         as AWS
import qualified Network.HTTP.Client          as HTTP

import Futurice.App.Avatar.Types

-- export handler
foreign export ccall avatarProcessLambda :: AwsLambdaHandler

newtype Config = Config
    { cfgS3Bucket :: Text
    }
  deriving (Show)

instance Configure Config where
    configure = Config
        <$> envVar "S3_BUCKET"

avatarProcessLambda :: AwsLambdaHandler
avatarProcessLambda = makeAwsLambda impl where
    impl :: Config -> Logger -> Manager -> AvatarProcess -> LogT IO (Either String Text)
    impl Config {..} _lgr mgr ap = do
        let digest = avatarProcessDigest ap
        logInfo "Processing" $ object
            [ "config" .= ap
            , "digest" .= digest
            ]

        req <- HTTP.parseUrlThrow (apUrl ap ^. unpacked)
        res <- liftIO $ HTTP.httpLbs req mgr
        let body = HTTP.responseBody res
        logTrace "Input image size" (BSL.length body)

        case avatar (apSize ap) (apGray ap) (BSL.toStrict body) of
            Left err  -> do
                logAttention "avatar failed" err
                return $ Left err
            Right img -> do
                logTrace "Avatar image size" $ BSL.length img
                env' <- AWS.newEnvWith AWS.Discover Nothing mgr
                let env = env' & AWS.envRegion .~ AWS.Frankfurt
                AWS.runResourceT $ AWS.runAWST env $ do
                    r <- AWS.send $ AWS.putObject
                        (AWS.BucketName cfgS3Bucket)
                        (AWS.ObjectKey digest)
                        (AWS.toBody img)
                    logTrace "AWS putObject" $ show r
                    return $ Right digest

-------------------------------------------------------------------------------
-- logic
-------------------------------------------------------------------------------

transformImage :: Int -> Image PixelRGBA8 -> Image PixelRGBA8
transformImage avatarSize = scale (avatarSize, avatarSize) . cropImage

cropImage :: Image PixelRGBA8 -> Image PixelRGBA8
cropImage img@(Image w h _) = trimImage img wh tl
  where
    (wh, tl) = toSquare (w, h)

toSquare :: (Int, Int) -> ((Int, Int), (Int, Int))
toSquare (w, h)
    | h > w     = ((w, w), (0              , ((h-w) `div` 5)))
    | otherwise = ((h, h), (((w-h) `div` 2), 0))

avatar :: Int -> Bool -> BS.ByteString -> Either String BSL.ByteString
avatar avatarSize grey contents = do
    origImg <- decodeImage contents
    convImg <- encodeDynamicPng $ constr $ transformImage avatarSize
        $ convertRGBA8 origImg
    return convImg
  where
    constr | grey      = ImageY8 . extractLumaPlane
           | otherwise = ImageRGBA8

-- | From JuicyPixels-util.
--  Copyright (C) 2012-2013 Fumiaki Kinoshita
trimImage :: Image PixelRGBA8
    -> (Int, Int) -- ^ width, height
    -> (Int, Int) -- ^ the left corner point
    -> Image PixelRGBA8
trimImage (Image w _ vec) (w', h') (x0, y0) =
    unsafePerformIO $ V.unsafeWith vec $ \ptr -> do
        mv <- MV.unsafeNew $ w' * h' * 4
        MV.unsafeWith mv $ \dst -> forM_ [0..h'-1] $ \y ->
            copyBytes (plusPtr dst $ y * w' * 4) (plusPtr ptr $ (*4) $ (y + y0) * w + x0) (4 * w')
        Image w' h' `fmap` V.unsafeFreeze mv
{-# NOINLINE trimImage #-}
