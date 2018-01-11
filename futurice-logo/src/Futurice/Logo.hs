{-# LANGUAGE TemplateHaskell    #-}
module Futurice.Logo (
    makeLogo,
    makeLogoBS,
    ) where

import           Codec.Picture        (Image, Pixel8, PixelRGBA8 (..),
                                       decodeImage, encodePng, mixWithAlpha,
                                       pixelMap, convertRGBA8)
import           Codec.Picture.Types  (promotePixel)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.FileEmbed
import           Data.Word

import Futurice.Colour

logoTemplate :: BS.ByteString
logoTemplate = $(makeRelativeToProject "futu-favicon-template.png" >>= embedFile)

logoTemplateImage :: Image PixelRGBA8
logoTemplateImage =
    case decodeImage logoTemplate of
        Right i  -> convertRGBA8 i
        Left err -> error $ "logoTemplateImage: " ++ err

type PixelRGBA8Triple = (PixelRGBA8, PixelRGBA8, PixelRGBA8)

processPixel :: PixelRGBA8Triple -> PixelRGBA8 -> PixelRGBA8
processPixel (kr, kg, kb) (PixelRGBA8 r g b _a) = colour
  where
    r' = multPixelRGBA8 r kr
    g' = multPixelRGBA8 g kg
    b' = multPixelRGBA8 b kb

    colour :: PixelRGBA8
    colour = r' `mix` g' `mix` b'

    mix :: PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8
    mix = mixWithAlpha (\_ -> boundedPlus) max

boundedPlus :: Word8 -> Word8 -> Word8
boundedPlus x y = fromIntegral $ min 255 $ x' + y'
  where x' = fromIntegral x :: Word16
        y' = fromIntegral y :: Word16

multPixel8 :: Pixel8 -> Pixel8 -> Pixel8
multPixel8 k x = fromIntegral $ div (fromIntegral k * x') 255
  where x' = fromIntegral x :: Word32

multPixelRGBA8 :: Pixel8 -> PixelRGBA8 -> PixelRGBA8
multPixelRGBA8 k (PixelRGBA8 r g b a) =
    PixelRGBA8 r' g' b' a'
  where r' = multPixel8 k r
        g' = multPixel8 k g
        b' = multPixel8 k b
        a' = multPixel8 k a

processImage :: PixelRGBA8Triple -> Image PixelRGBA8 -> Image PixelRGBA8
processImage ct = pixelMap (processPixel ct)

processLogo :: Colour -- ^ Disk colour
            -> Image PixelRGBA8
            -> Image PixelRGBA8
processLogo bg = processImage (bg', white, transparent)
    where bg' = promotePixel $ colourRGB8 bg
          white = PixelRGBA8 255 255 255 255
          transparent = PixelRGBA8 0 0 0 0

makeLogo :: Colour -- ^ disk colour
         -> Image PixelRGBA8
makeLogo bg = processLogo bg logoTemplateImage

makeLogoBS :: Colour -> BS.ByteString
makeLogoBS = BSL.toStrict . encodePng . makeLogo
