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
import Linear                 (M33, V3 (..))
import Prelude ()
import System.IO.Unsafe       (unsafePerformIO)

import qualified Control.Monad.Trans.AWS      as AWS
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV
import qualified Network.AWS.S3.PutObject     as AWS
import qualified Network.AWS.S3.Types         as AWS
import qualified Network.HTTP.Client          as HTTP

import Futurice.App.Avatar.Types

newtype Config = Config
    { cfgS3Bucket :: Text
    }
  deriving (Show)

instance Configure Config where
    configure = Config
        <$> envVar "S3_BUCKET"

avatarProcessLambda :: AwsLambdaHandler
avatarProcessLambda = makeAwsLambda impl where
    impl :: LambdaContext -> AWS.Env -> Config -> Logger -> Manager -> AvatarProcess -> LogT IO (Either String Text)
    impl _ env Config {..} _lgr mgr ap = do
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
transformImage avatarSize = convoluteM33 mask . scale (avatarSize, avatarSize) . cropImage
  where
    mask (V3 (V3 y0 z0 y1) (V3 z1 x z2) (V3 y2 z3 y3)) = truncate $ clamp $ sum
        [ k0 * fromIntegral x
        , k1 * fromIntegral z0
        , k1 * fromIntegral z1
        , k1 * fromIntegral z2
        , k1 * fromIntegral z3
        , k2 * fromIntegral y0
        , k2 * fromIntegral y1
        , k2 * fromIntegral y2
        , k2 * fromIntegral y3
        ]
      where
        k0 = 1.75 :: Double
        k1 = -0.125 -- 2/16
        k2 = -0.0625 -- 1/16

    clamp = max 0 . min 255

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

convoluteM33 :: (M33 Word8 -> Word8) -> Image PixelRGBA8 -> Image PixelRGBA8
convoluteM33 k (Image w h d) = Image w h $ V.imap f d where
    f i p11 = k (V3 (V3 p00 p01 p02) (V3 p10 p11 p12) (V3 p20 p21 p22))
      where
        (xy, o) = i `divMod` 4
        (y, x)  = xy `divMod` w

        x_ = max 0 (x - 1)
        x' = min (w - 1) (x + 1)

        y_ = max 0 (y - 1)
        y' = min (h - 1) (y + 1)

        p10 = d V.! (4 * (y * w + x_) + o)
        p12 = d V.! (4 * (y * w + x') + o)

        p00 = d V.! (4 * (y_ * w + x_) + o)
        p01 = d V.! (4 * (y_ * w + x ) + o)
        p02 = d V.! (4 * (y_ * w + x') + o)

        p20 = d V.! (4 * (y' * w + x_) + o)
        p21 = d V.! (4 * (y' * w + x ) + o)
        p22 = d V.! (4 * (y' * w + x') + o)


