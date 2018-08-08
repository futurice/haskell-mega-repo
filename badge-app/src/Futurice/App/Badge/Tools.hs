{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Futurice.App.Badge.Tools where

import Codec.Picture
       (DynamicImage (ImageRGBA8), Pixel, Image (..), convertRGBA8, decodeImage)
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Void                  (Void, absurd)
import Diagrams.Prelude
       (Additive, Any, DImage, Embedded, Enveloped, FillRule (EvenOdd),
       HasLinearMap, InSpace, Path, QDiagram, Renderable, SizeSpec,
       Transformable, Transformation, V2, fc, fillRule, lcA, requiredScale,
       scaling, size, transform, transparent, white)
import Diagrams.TwoD.Image        (embeddedImage)
import Futurice.Prelude
import Graphics.SVGFonts
import Graphics.SVGFonts.ReadFont (PreparedFont, loadFont')
import Prelude ()
import Codec.Picture.Extra (crop)

import qualified Codec.Archive.Tar    as Tar
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict      as Map

-------------------------------------------------------------------------------
-- Spec dimensions
-------------------------------------------------------------------------------

-- Portrait!

-- | CR-80 width in milli-inches
cr80Width :: Int
cr80Width = 2125

cr80Height :: Int
cr80Height = 3370

dpi :: Int
dpi = 300

fromMilliIn :: Int -> Double
fromMilliIn x = fromIntegral (x * dpi) / 1000.0

-------------------------------------------------------------------------------
-- Derived dimensions
-------------------------------------------------------------------------------

maxWidth :: Double
maxWidth = fromMilliIn cr80Width

maxHeight :: Double
maxHeight = fromMilliIn cr80Height

padding :: Double
padding = fromMilliIn 250

-------------------------------------------------------------------------------
-- Filesystem abstraction
-------------------------------------------------------------------------------

class Monad m => MonadReadOnlyFS m where
    readOnly :: FilePath -> m BS.ByteString

instance MonadReadOnlyFS IO where
    readOnly fp = do
        putStrLn $ "Reading file " ++ fp
        BS.readFile fp

data LoadingException
    = ImageLoadingFailed FilePath String
    | FileNotExists FilePath
  deriving (Eq, Show)

instance Exception LoadingException

-------------------------------------------------------------------------------
-- TarM
-------------------------------------------------------------------------------

newtype TarM a = TarM (ReaderT (Map FilePath BS.ByteString) (Either SomeException) a)
  deriving newtype (Functor, Applicative, Monad, MonadThrow)

runTarM :: MonadThrow m => Tar.Entries Void -> TarM a -> m a
runTarM entries (TarM m) = either throwM return $ runReaderT m files
  where
    files = Map.fromList $
        Tar.foldEntries (\e -> maybe id (:) (fileEntry e)) [] absurd entries

    fileEntry :: Tar.Entry -> Maybe (FilePath, BS.ByteString)
    fileEntry entry = case Tar.entryContent entry of
        Tar.NormalFile lbs _ -> Just (Tar.entryPath entry, LBS.toStrict lbs)
        _                    -> Nothing


instance MonadReadOnlyFS TarM where
    readOnly fp = TarM $ ReaderT $ \m -> case m ^. at fp of
        Nothing -> throwM $ FileNotExists fp
        Just bs -> return bs

removeTarErrors :: Show e => Tar.Entries e -> Either String (Tar.Entries a)
removeTarErrors (Tar.Fail e)    = Left (show e)
removeTarErrors Tar.Done        = pure Tar.Done
removeTarErrors (Tar.Next x xs) = Tar.Next x <$> removeTarErrors xs

-------------------------------------------------------------------------------
-- Data loaders
-------------------------------------------------------------------------------

-- | Loads not only PNGs.
loadImageEmbRGBA8
    :: (MonadReadOnlyFS m, MonadThrow m)
    => FilePath
    -> m (DImage Double Embedded)
loadImageEmbRGBA8 path = do
    bs <- readOnly path
    di <- either (throwM . ImageLoadingFailed path) pure (decodeImage bs)
    return $ fromDynamicImage di

fromDynamicImage :: DynamicImage -> DImage Double Embedded
fromDynamicImage = embeddedImage . ImageRGBA8 . convertRGBA8

loadFontRO
    :: (MonadReadOnlyFS m, MonadThrow m)
    => FilePath
    -> m (PreparedFont Double)
loadFontRO path = do
    bs <- readOnly path
    let (_ws, pf) = loadFont' "Font" bs
    return pf

-------------------------------------------------------------------------------
-- JuicyPixels helpers
-------------------------------------------------------------------------------

cropImage :: Pixel a => Image a -> Image a
cropImage img
    -- w / cr80Width > h / cr80Height
    | w * cr80Height > h * cr80Width =
        crop (div (w - w') 2) 0 w' h img
    -- otherwise
    | otherwise =
        crop 0 (div (h - h') 2) w h' img
  where
    w = imageWidth img
    h = imageHeight img

    w' = h * cr80Width `div` cr80Height
    h' = w * cr80Height `div` cr80Width

-------------------------------------------------------------------------------
-- Diagrams Helpers
-------------------------------------------------------------------------------

-- | 'sized' which doesn't enlarge.
sized'
    :: (InSpace v n a, HasLinearMap v, Transformable a, Enveloped a)
    => SizeSpec v n -> a -> a
sized' spec a = transform (requiredScaling' spec (size a)) a
  where
    requiredScaling'
        :: (Additive v, Foldable v, Fractional n, Ord n)
        => SizeSpec v n -> v n -> Transformation v n
    requiredScaling' sp = scaling . min 1 . requiredScale sp

-- | ... using a local font
renderText
    :: Renderable (Path V2 Double) b
    => PreparedFont Double  -- ^ font
    -> Double               -- ^ size
    -> String               -- ^ text
    -> QDiagram b V2 Double Any
renderText f s t = textSVG_ (TextOpts f INSIDE_H KERN False (maxWidth - padding) s) t
    & fc white
    & lcA transparent
    & fillRule EvenOdd
    -- & showOrigin
    -- & showEnvelope
