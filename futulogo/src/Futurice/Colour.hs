{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE Trustworthy         #-}
-- |
-- Module      :  Futurice.Colour
-- License     :  BSD-3-Clause (see the file LICENSE)
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- See the non-public (?) brand guidelines
module Futurice.Colour (
    -- * Colours
    Colour(..),
    AccentFamily(..),
    AccentColour(..),
    -- * Conversions
    colourName,
    colourPantoneName,
    colourRGB8,
    colourCMYK8,
    colourClay,
    -- * Colour Combinations
    infographicsColours,
    ColourCombination(..),
    ColourTriple,
    colourCombinationTriple,
    -- * Type level trickery
    SColour(..),
    SAccentFamily(..),
    SAccentColour(..),
    ) where

import Codec.Picture.Types (PixelRGB8 (..), PixelCMYK8(..))
import Control.DeepSeq     (NFData)
import Data.Hashable       (Hashable)
import Data.Monoid         ((<>))
import Data.Tagged         (Tagged (..), untag)
import Data.Text           (Text)
import Data.Typeable       (Typeable)
import GHC.Generics        (Generic)
#if MIN_VERSION_servant(0,5,0)
import Servant.API         (FromHttpApiData (..), ToHttpApiData (..))
#else
import Servant.Common.Text (FromText (..), ToText (..))
#endif
import Clay.Color (Color, rgba)

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

data AccentFamily
    = AF1  -- ^ Greens
    | AF2  -- ^ Violets
    | AF3  -- ^ Yellow, orange, red
    | AF4  -- ^ Light warms
    | AF5  -- ^ Light colds
    | AF6  -- ^ Light colourfuls
    deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic, Typeable)

instance NFData AccentFamily
instance Hashable AccentFamily

data AccentColour
    = AC1 | AC2 | AC3
    deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic, Typeable)

instance NFData AccentColour
instance Hashable AccentColour

data Colour
    = FutuGreen       -- ^ Futurice Green
    | FutuBlack       -- ^ Black
    | FutuLightGreen  -- ^ Accent Green Light
    | FutuDarkGreen   -- ^ Accent Green Dark
    | FutuAccent AccentFamily AccentColour
      -- ^ Accent Colours
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

instance Enum Colour where
    toEnum 0 = FutuGreen
    toEnum 1 = FutuBlack
    toEnum 2 = FutuLightGreen
    toEnum 3 = FutuDarkGreen
    toEnum n = FutuAccent (toEnum f) (toEnum c)
      where (f, c) = quotRem (n - 4) 3

    fromEnum FutuGreen        = 0
    fromEnum FutuBlack        = 1
    fromEnum FutuLightGreen   = 2
    fromEnum FutuDarkGreen    = 3
    fromEnum (FutuAccent f c) = 4 + 3 * fromEnum f + fromEnum c

instance Bounded Colour where
    minBound = FutuGreen
    maxBound = FutuAccent maxBound maxBound

instance NFData Colour
instance Hashable Colour

------------------------------------------------------------------------------
-- Colour conversions
------------------------------------------------------------------------------

-- | Colour name.
colourName :: Colour -> Maybe Text
colourName FutuGreen      = Just "Futurice Green"
colourName FutuBlack      = Just "Black"
colourName FutuLightGreen = Just "Accent Green Light"
colourName FutuDarkGreen  = Just "Accent Green Dark"
colourName _              = Nothing

-- | Colour pantone name.
colourPantoneName :: Colour -> Text
colourPantoneName FutuGreen             = "Pantone 7740"
colourPantoneName FutuBlack             = "Pantone Black 4"
colourPantoneName FutuLightGreen        = "Pantone 7738"
colourPantoneName FutuDarkGreen         = "Pantone 7743"
colourPantoneName (FutuAccent AF1 AC1)  = "Pantone 574"
colourPantoneName (FutuAccent AF1 AC2)  = "Pantone 561"
colourPantoneName (FutuAccent AF1 AC3)  = "Pantone 548"
colourPantoneName (FutuAccent AF2 AC1)  = "Pantone 7444"
colourPantoneName (FutuAccent AF2 AC2)  = "Pantone Violet"
colourPantoneName (FutuAccent AF2 AC3)  = "Pantone 261"
colourPantoneName (FutuAccent AF3 AC1)  = "Pantone 113"
colourPantoneName (FutuAccent AF3 AC2)  = "Pantone 151"
colourPantoneName (FutuAccent AF3 AC3)  = "Pantone Warm Red"
colourPantoneName (FutuAccent AF4 AC1)  = "Pantone Warm Gray"
colourPantoneName (FutuAccent AF4 AC2)  = "Pantone 482"
colourPantoneName (FutuAccent AF4 AC3)  = "Pantone 410"
colourPantoneName (FutuAccent AF5 AC1)  = "Pantone 7541"
colourPantoneName (FutuAccent AF5 AC2)  = "Pantone 434"
colourPantoneName (FutuAccent AF5 AC3)  = "Pantone 430"
colourPantoneName (FutuAccent AF6 AC1)  = "Pantone 7499"
colourPantoneName (FutuAccent AF6 AC2)  = "Pantone 600"
colourPantoneName (FutuAccent AF6 AC3)  = "Pantone 7485"

-- | Convert to JuicyPixels colour
colourRGB8 :: Colour -> PixelRGB8
colourRGB8 FutuGreen             = PixelRGB8 50 158 65
colourRGB8 FutuBlack             = PixelRGB8 33 15 0
colourRGB8 FutuLightGreen        = PixelRGB8 65 175 70
colourRGB8 FutuDarkGreen         = PixelRGB8 38 104 38
colourRGB8 (FutuAccent AF1 AC1)  = PixelRGB8 205 236 228
colourRGB8 (FutuAccent AF1 AC2)  = PixelRGB8 0 90 75
colourRGB8 (FutuAccent AF1 AC3)  = PixelRGB8 0 52 65
colourRGB8 (FutuAccent AF2 AC1)  = PixelRGB8 190 195 230
colourRGB8 (FutuAccent AF2 AC2)  = PixelRGB8 70 40 154
colourRGB8 (FutuAccent AF2 AC3)  = PixelRGB8 80 10 90
colourRGB8 (FutuAccent AF3 AC1)  = PixelRGB8 255 240 70
colourRGB8 (FutuAccent AF3 AC2)  = PixelRGB8 255 130 30
colourRGB8 (FutuAccent AF3 AC3)  = PixelRGB8 255 82 64
colourRGB8 (FutuAccent AF4 AC1)  = PixelRGB8 242 238 230
colourRGB8 (FutuAccent AF4 AC2)  = PixelRGB8 240 214 195
colourRGB8 (FutuAccent AF4 AC3)  = PixelRGB8 114 98 95
colourRGB8 (FutuAccent AF5 AC1)  = PixelRGB8 238 243 245
colourRGB8 (FutuAccent AF5 AC2)  = PixelRGB8 255 220 220
colourRGB8 (FutuAccent AF5 AC3)  = PixelRGB8 126 135 139
colourRGB8 (FutuAccent AF6 AC1)  = PixelRGB8 255 240 210
colourRGB8 (FutuAccent AF6 AC2)  = PixelRGB8 255 245 175
colourRGB8 (FutuAccent AF6 AC3)  = PixelRGB8 230 245 220

-- | Convert to JuicyPixels CMYK colour.
--
-- /Note:/ uses 'colourRGB8. TODO: weite explicitly
colourCMYK8 :: Colour -> PixelCMYK8
colourCMYK8 FutuGreen             = PixelCMYK8 75 0 95 15
colourCMYK8 FutuBlack             = PixelCMYK8 41 57 72 90
colourCMYK8 FutuLightGreen        = PixelCMYK8 74 80 98 2
colourCMYK8 FutuDarkGreen         = PixelCMYK8 71 8 100 50
colourCMYK8 (FutuAccent AF1 AC1)  = PixelCMYK8 20 0 14 0
colourCMYK8 (FutuAccent AF1 AC2)  = PixelCMYK8 84 20 58 54
colourCMYK8 (FutuAccent AF1 AC3)  = PixelCMYK8 100 21 28 76
colourCMYK8 (FutuAccent AF2 AC1)  = PixelCMYK8 27 21 0 0
colourCMYK8 (FutuAccent AF2 AC2)  = PixelCMYK8 90 99 0 0
colourCMYK8 (FutuAccent AF2 AC3)  = PixelCMYK8 62 100 9 44
colourCMYK8 (FutuAccent AF3 AC1)  = PixelCMYK8 0 2 83 0
colourCMYK8 (FutuAccent AF3 AC2)  = PixelCMYK8 0 60 100 0
colourCMYK8 (FutuAccent AF3 AC3)  = PixelCMYK8 0 83 80 0
colourCMYK8 (FutuAccent AF4 AC1)  = PixelCMYK8 3 3 6 7
colourCMYK8 (FutuAccent AF4 AC2)  = PixelCMYK8 4 17 21 7
colourCMYK8 (FutuAccent AF4 AC3)  = PixelCMYK8 22 33 28 60
colourCMYK8 (FutuAccent AF5 AC1)  = PixelCMYK8 7 1 3 2
colourCMYK8 (FutuAccent AF5 AC2)  = PixelCMYK8 5 11 8 12
colourCMYK8 (FutuAccent AF5 AC3)  = PixelCMYK8 33 18 13 40
colourCMYK8 (FutuAccent AF6 AC1)  = PixelCMYK8 1 2 24 0
colourCMYK8 (FutuAccent AF6 AC2)  = PixelCMYK8 2 0 39 0
colourCMYK8 (FutuAccent AF6 AC3)  = PixelCMYK8 9 0 18 0

colourClay :: Colour -> Color
colourClay = f . colourRGB8
  where
    f (PixelRGB8 r g b) = rgba (toInteger r) (toInteger g) (toInteger b) 255

------------------------------------------------------------------------------
-- Colour combinations
------------------------------------------------------------------------------

-- | Bright colours in infographics
infographicsColours :: [Colour]
infographicsColours =
    [ FutuGreen
    , FutuAccent AF3 AC2
    , FutuAccent AF2 AC2
    , FutuAccent AF3 AC3
    , FutuAccent AF1 AC2
    , FutuAccent AF3 AC1
    ]

data ColourCombination
    = SpringDay | Spruce | Forest
    | LateWnter | Eskimo | GasFlame
    | South | BeachSand | Morning
    | Kiwi | Melon | Plum
    deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic, Typeable)

instance NFData ColourCombination
instance Hashable ColourCombination

type ColourTriple = (Colour, Colour, Colour)

colourCombinationTriple :: ColourCombination -> ColourTriple
colourCombinationTriple SpringDay = (toEnum 2, toEnum 10, toEnum 4)
-- TODO: implement me
colourCombinationTriple _ = (toEnum 0, toEnum 0, toEnum 0)
{-
springDay, spruce, forest :: ColourCombination
lateWinter, eskimo, gasFlame :: ColourCombination
south, beachSand, morning :: ColourCombination
kiwi, melon, plum :: ColourCombination

springDay   = (toEnum 2, toEnum 10, toEnum 4)
spruce      = undefined
forest      = undefined
lateWinter  = undefined
eskimo      = undefined
gasFlame    = undefined
south       = undefined
beachSand   = undefined
morning     = undefined
kiwi        = undefined
melon       = undefined
plum        = undefined
-}

------------------------------------------------------------------------------
-- Type level trickery
------------------------------------------------------------------------------

class SAccentColour (c :: AccentColour) where
    saccentcolour :: Tagged c AccentColour

instance SAccentColour 'AC1 where saccentcolour = Tagged AC1
instance SAccentColour 'AC2 where saccentcolour = Tagged AC2
instance SAccentColour 'AC3 where saccentcolour = Tagged AC3

class SAccentFamily (f :: AccentFamily) where
    saccentfamily :: Tagged f AccentFamily

instance SAccentFamily 'AF1 where saccentfamily = Tagged AF1
instance SAccentFamily 'AF2 where saccentfamily = Tagged AF2
instance SAccentFamily 'AF3 where saccentfamily = Tagged AF3
instance SAccentFamily 'AF4 where saccentfamily = Tagged AF4
instance SAccentFamily 'AF5 where saccentfamily = Tagged AF5
instance SAccentFamily 'AF6 where saccentfamily = Tagged AF6

class SColour (c :: Colour) where
    scolour :: Tagged c Colour

instance SColour 'FutuGreen where scolour       = Tagged FutuGreen
instance SColour 'FutuBlack where scolour       = Tagged FutuGreen
instance SColour 'FutuLightGreen where scolour  = Tagged FutuLightGreen
instance SColour 'FutuDarkGreen where scolour   = Tagged FutuDarkGreen

instance (SAccentColour c, SAccentFamily f) => SColour ('FutuAccent f c) where
    scolour = Tagged $ FutuAccent (untag f) (untag c)
      where
        f = saccentfamily :: Tagged f AccentFamily
        c = saccentcolour :: Tagged c AccentColour

------------------------------------------------------------------------------
-- Servant bindings
------------------------------------------------------------------------------

#if MIN_VERSION_servant(0,5,0)
instance ToHttpApiData AccentFamily where
    toUrlPiece AF1 = "f1"
    toUrlPiece AF2 = "f2"
    toUrlPiece AF3 = "f3"
    toUrlPiece AF4 = "f4"
    toUrlPiece AF5 = "f5"
    toUrlPiece AF6 = "f6"

instance ToHttpApiData AccentColour where
    toUrlPiece AC1 = "c1"
    toUrlPiece AC2 = "c2"
    toUrlPiece AC3 = "c3"

instance ToHttpApiData Colour where
    toUrlPiece FutuGreen         = "futu-green"
    toUrlPiece FutuBlack         = "futu-black"
    toUrlPiece FutuLightGreen    = "futu-light-green"
    toUrlPiece FutuDarkGreen     = "futu-dark-green"
    toUrlPiece (FutuAccent f c)  = "futu-" <> toUrlPiece f <> "-" <> toUrlPiece c

instance FromHttpApiData AccentFamily where
    parseUrlPiece t = maybe (Left $ "Invalid accent family: " <> t) Right $ lookup t ts
      where ts = [ (toUrlPiece c, c) | c <- [minBound..maxBound] ]

instance FromHttpApiData AccentColour where
    parseUrlPiece t = maybe (Left $ "Invalid accent colour: " <> t) Right $ lookup t ts
      where ts = [ (toUrlPiece c, c) | c <- [minBound..maxBound] ]

instance FromHttpApiData Colour where
    parseUrlPiece t = maybe (Left $ "Invalid colour: " <> t) Right $ lookup t ts
      where ts = [ (toUrlPiece c, c) | c <- [minBound..maxBound] ]
#else
instance ToText AccentFamily where
    toText AF1 = "f1"
    toText AF2 = "f2"
    toText AF3 = "f3"
    toText AF4 = "f4"
    toText AF5 = "f5"
    toText AF6 = "f6"

instance ToText AccentColour where
    toText AC1 = "c1"
    toText AC2 = "c2"
    toText AC3 = "c3"

instance ToText Colour where
    toText FutuGreen         = "futu-green"
    toText FutuBlack         = "futu-black"
    toText FutuLightGreen    = "futu-light-green"
    toText FutuDarkGreen     = "futu-dark-green"
    toText (FutuAccent f c)  = "futu-" <> toText f <> "-" <> toText c

instance FromText AccentFamily where
    fromText = flip lookup t
      where t = [ (toText c, c) | c <- [minBound..maxBound] ]

instance FromText AccentColour where
    fromText = flip lookup t
      where t = [ (toText c, c) | c <- [minBound..maxBound] ]

instance FromText Colour where
    fromText = flip lookup t
      where t = [ (toText c, c) | c <- [minBound..maxBound] ]
#endif

------------------------------------------------------------------------------
-- Typeable lifted
------------------------------------------------------------------------------

deriving instance Typeable 'AC1
deriving instance Typeable 'AC2
deriving instance Typeable 'AC3

deriving instance Typeable 'AF1
deriving instance Typeable 'AF2
deriving instance Typeable 'AF3
deriving instance Typeable 'AF4
deriving instance Typeable 'AF5
deriving instance Typeable 'AF6

deriving instance Typeable 'FutuGreen
deriving instance Typeable 'FutuBlack
deriving instance Typeable 'FutuLightGreen
deriving instance Typeable 'FutuDarkGreen
deriving instance Typeable 'FutuAccent
