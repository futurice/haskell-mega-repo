{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
-- |
-- Module      :  Servant.Chart
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- An 'SVG' empty data type with 'MimeRender' instances for @Chart@.
--
-- >>> type ChartGet a = Get '[SVG] a
module Servant.Chart (SVG, Chart (..)) where

import Data.FileEmbed (makeRelativeToProject)
import Data.Maybe     (fromMaybe)
import Data.Proxy     (Proxy (..))
import Data.String    (fromString)
import Data.Swagger   (NamedSchema (..), ToSchema (..))
import Data.Typeable  (Typeable)
import FileEmbedLzma  (embedDir)
import GHC.TypeLits   (KnownSymbol, Symbol, symbolVal)
import Servant.API    (Accept (..), MimeRender (..))

import Graphics.Rendering.Chart.Backend.Diagrams
       (DEnv, FontSelector, createEnv, runBackendR)
import Graphics.Rendering.Chart.Renderable       (ToRenderable (..))

import qualified Diagrams                         as D
import qualified Diagrams.Backend.SVG             as DSVG
import qualified Graphics.Rendering.Chart.Backend as B
import qualified Graphics.Rendering.Chart.Easy    as C
import qualified Graphics.Svg.Core                as S
import qualified Graphics.SVGFonts.ReadFont       as F
import qualified Network.HTTP.Media               as M

import System.IO.Unsafe (unsafePerformIO)

#ifdef MIN_VERSION_compact
import           Data.Compact
                 (compactAddWithSharing, compactSize, compactWithSharing,
                 getCompact)
import qualified Data.Vector  as V
#endif

-------------------------------------------------------------------------------
-- Servant
-------------------------------------------------------------------------------

data SVG deriving Typeable

-- | @image/svg+xml@
instance Accept SVG where
    contentType _ = "image" M.// "svg+xml"

instance {-# OVERLAPPABLE #-} ToRenderable a => MimeRender SVG a where
    mimeRender _
        = S.renderBS
        . D.renderDia DSVG.SVG opts
        . fst
        . runBackendR denv
        . toRenderable
      where
        opts = DSVG.SVGOptions
            { DSVG._size            = D.dims2D w h
            , DSVG._svgDefinitions  = Nothing
            , DSVG._idPrefix        = mempty
            , DSVG._svgAttributes   = []
            , DSVG._generateDoctype = True
            }
        -- TODO: make configurable
        w = 1000
        h = 700

-------------------------------------------------------------------------------
-- Internals
-------------------------------------------------------------------------------

denv :: DEnv Double
denv = createEnv B.vectorAlignmentFns 1000 700 loadSansSerifFonts

loadSansSerifFonts :: FontSelector Double
loadSansSerifFonts = unsafePerformIO loadSansSerifFontsIO
{-# NOINLINE loadSansSerifFonts #-}

loadSansSerifFontsIO :: IO (FontSelector Double)
loadSansSerifFontsIO = do
    let sansR' :: F.PreparedFont Double
        sansR'   = snd $ F.loadFont' "SourceSansPro_R"   $ sureLookup "/SourceSansPro_R.svg"    fontsDir

        sansRB' :: F.PreparedFont Double
        sansRB'  = snd $ F.loadFont' "SourceSansPro_RB"  $ sureLookup "/SourceSansPro_RB.svg"   fontsDir

        sansRBI' :: F.PreparedFont Double
        sansRBI' = snd $ F.loadFont' "SourceSansPro_RBI" $ sureLookup  "/SourceSansPro_RBI.svg" fontsDir

        sansRI' :: F.PreparedFont Double
        sansRI'  = snd $ F.loadFont' "SourceSansPro_RI"  $ sureLookup "/SourceSansPro_RI.svg"   fontsDir

#ifdef MIN_VERSION_compact
    regionR   <- compactWithSharing              (forceFontData $ fst sansR')
    regionRB  <- compactAddWithSharing regionR   (forceFontData $ fst sansRB')
    regionRBI <- compactAddWithSharing regionRB  (forceFontData $ fst sansRBI')
    regionRI  <- compactAddWithSharing regionRBI (forceFontData $ fst sansRI')

    print =<< compactSize regionRI

    let sansR   = (getCompact regionR,   snd sansR')
    let sansRB  = (getCompact regionRB,  snd sansRB')
    let sansRBI = (getCompact regionRBI, snd sansRBI')
    let sansRI  = (getCompact regionRI,  snd sansRI')
#else
    let sansR   = sansR'
    let sansRB  = sansRB'
    let sansRBI = sansRBI'
    let sansRI  = sansRI'
#endif

    return (selectFont sansR sansRB sansRBI sansRI)
  where
    fontsDir = $(makeRelativeToProject "fonts" >>= embedDir)

#ifdef MIN_VERSION_compact
    forceFontData :: F.FontData Double -> F.FontData Double
    forceFontData fd = fd { F.fontDataKerning = kern' } where
        kern = F.fontDataKerning fd
        kern' = kern { F.kernK = V.force $ F.kernK kern }
#endif

    sureLookup :: (Eq a, Show a) => a -> [(a, b)] -> b
    sureLookup a xs = fromMaybe
        (error $ "Cannot find " ++ show a ++ " in " ++ show (map fst xs))
        (lookup a xs)

    selectFont
        :: F.PreparedFont Double -> F.PreparedFont Double
        -> F.PreparedFont Double -> F.PreparedFont Double
        -> B.FontStyle -> F.PreparedFont Double
    selectFont sansR sansRB sansRBI sansRI fs = case (B._font_name fs, B._font_slant fs, B._font_weight fs) of
        (_, B.FontSlantNormal , B.FontWeightNormal) -> alterFontFamily "sans-serif" sansR
        (_, B.FontSlantNormal , B.FontWeightBold  ) -> alterFontFamily "sans-serif" sansRB
        (_, B.FontSlantItalic , B.FontWeightNormal) -> alterFontFamily "sans-serif" sansRI
        (_, B.FontSlantOblique, B.FontWeightNormal) -> alterFontFamily "sans-serif" sansRI
        (_, B.FontSlantItalic , B.FontWeightBold  ) -> alterFontFamily "sans-serif" sansRBI
        (_, B.FontSlantOblique, B.FontWeightBold  ) -> alterFontFamily "sans-serif" sansRBI

alterFontFamily :: String -> F.PreparedFont n -> F.PreparedFont n
alterFontFamily n (fd, om) = (fd { F.fontDataFamily = n }, om)

-------------------------------------------------------------------------------
-- Chart
-------------------------------------------------------------------------------

newtype Chart (name :: Symbol) = Chart (C.Renderable ())

instance C.ToRenderable (Chart name) where
    toRenderable (Chart r) = r

instance KnownSymbol name => ToSchema (Chart name) where
    declareNamedSchema _ = pure $ NamedSchema (Just $ fromString $ "Chart" ++ n) mempty
      where
        n = symbolVal (Proxy :: Proxy name)
