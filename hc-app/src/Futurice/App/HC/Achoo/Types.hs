{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Futurice.App.HC.Achoo.Types where

import Data.Monoid               (Sum)
import Data.Time                 (diffDays)
import Data.Vec.Lazy.Inline      (Vec (..))
import Futurice.Company          (Country)
import Futurice.Generics
import Futurice.Office           (Office)
import Futurice.Prelude
import Futurice.Tribe            (Tribe)
import Numeric.Interval.NonEmpty (Interval, inf, sup)
import Prelude ()

import qualified Data.Type.Nat   as N
import qualified Futurice.Colour as FC

type GraphBucketsN = N.Nat8
type TableBucketsN = N.Nat5

type PerSickDays a = Vec GraphBucketsN (Sum a)

bucketNames :: Vec GraphBucketsN String
bucketNames =
    "0"     :::
    "1"     :::
    "2"     :::
    "3"     :::
    "4-5"   :::
    "6-10"  :::
    "11-30" :::
    "31+"   :::
    VNil

tableBucketNames :: Vec TableBucketsN String
tableBucketNames =
    "zero"   :::
    "1-3"    :::
    "3-10"   :::
    "11-30"  :::
    "31+"    :::
    VNil

bucketColors :: Vec GraphBucketsN FC.Colour
bucketColors =
    FC.FutuGreen :::
    FC.FutuAccent FC.AF1 FC.AC3 :::
    FC.FutuAccent FC.AF3 FC.AC3 :::
    FC.FutuAccent FC.AF2 FC.AC3 :::
    FC.FutuAccent FC.AF2 FC.AC1 :::
    FC.FutuAccent FC.AF5 FC.AC3 :::
    FC.FutuAccent FC.AF4 FC.AC2 :::
    FC.FutuAccent FC.AF4 FC.AC3 :::
    VNil

toTableBuckets :: Semigroup a => Vec GraphBucketsN a -> Vec TableBucketsN a
toTableBuckets (a ::: b ::: c ::: d ::: e ::: f ::: rest) =
    a ::: (b <> c <> d) ::: (e <> f) ::: rest

data AchooReport = AchooReport
    { arInterval       :: Interval Day
    , arWhole          :: Bool

    , arPercentsTribe   :: Map Tribe   (PerSickDays Int)
    , arPercentsOffice  :: Map Office  (PerSickDays Int)
    , arPercentsCountry :: Map (Maybe Country) (PerSickDays Int)

    , arAverageTribe   :: Map Tribe   Double
    , arAverageOffice  :: Map Office  Double
    , arAverageCountry :: Map (Maybe Country) Double

    , arDistrTribe   :: Map Tribe (Map Duration Int)
    , arDistrOffice  :: Map Office (Map Duration Int)
    , arDistrCountry :: Map (Maybe Country) (Map Duration Int)
    }
  deriving Generic

instance NFData AchooReport

-------------------------------------------------------------------------------
-- Duration
-------------------------------------------------------------------------------

newtype Duration = Duration Int
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (NFData)

instance Show Duration where
    showsPrec d (Duration n) = showsPrec d n

instance Semigroup Duration where
    Duration n <> Duration m = Duration (n + m)

instance Monoid Duration where
    mempty  = Duration 0
    mappend = (<>)

durationOfInterval :: Interval Day -> Duration
durationOfInterval i = Duration $ fromInteger $ diffDays (sup i) (inf i)

-------------------------------------------------------------------------------
-- Chart selector
-------------------------------------------------------------------------------

data AchooChart
    = ACTribe
    | ACOffice
    | ACCountry
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)
  deriving anyclass (NFData, Binary)

makePrisms ''AchooChart
deriveGeneric ''AchooChart
deriveLift ''AchooChart

instance TextEnum AchooChart where
    type TextEnumNames AchooChart =
        '["tribe"
        , "office"
        , "company"
        ]

deriveVia [t| ToHttpApiData AchooChart   `Via` Enumica AchooChart |]
deriveVia [t| FromHttpApiData AchooChart `Via` Enumica AchooChart |]
instance ToParamSchema AchooChart where toParamSchema = enumToParamSchema
instance ToSchema AchooChart where declareNamedSchema = enumDeclareNamedSchema
