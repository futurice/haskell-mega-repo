{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.HC.EarlyCaring.Types where

import Algebra.Lattice           ((/\))
import Control.Lens              (alaf)
import Data.Aeson
       (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Fixed                (Deci)
import Data.Swagger              (NamedSchema (..), ToSchema (..))
import Data.Time                 (addDays, diffDays)
import Futurice.Email            (Email)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Futurice.Time
import Numeric.Interval.NonEmpty (Interval, inf, sup)
import Prelude ()

import qualified Crypto.Hash.SHA512                   as SHA512
import qualified Data.Binary                          as Binary
import qualified Data.ByteString.Base16               as Base16
import qualified Data.ByteString.Base64.URL.Lazy.Type as LBS64
import qualified Data.ByteString.Base64.URL.Type      as BS64
import qualified Data.Map.Strict                      as Map
import qualified Data.RangeSet.List                   as RSet
import qualified Personio                             as P

-------------------------------------------------------------------------------
-- Signed
-------------------------------------------------------------------------------

data SignedBlob a = SignedBlob
    { sbData :: LazyByteString
    , sbHmac :: ByteString
    }
  deriving (Show)

-- TODO
instance ToSchema (SignedBlob a) where
    declareNamedSchema _ = return (NamedSchema (Just "SignedBlob") mempty)

instance ToJSON (SignedBlob a) where
    toJSON (SignedBlob lbs hmac) = object
        [ "data" .= LBS64.makeByteString64 lbs
        , "hmac" .= BS64.makeByteString64 hmac
        ]

instance FromJSON (SignedBlob a) where
    parseJSON = withObject "SignedBlob" $ \obj -> SignedBlob
        <$> fmap LBS64.getByteString64 (obj .: "data")
        <*> fmap BS64.getByteString64  (obj .: "hmac")

mkSignedBlob :: Binary a => ByteString -> a -> SignedBlob a
mkSignedBlob secret x = SignedBlob
    { sbData = lbs
    , sbHmac = SHA512.hmaclazy secret lbs
    }
  where
    lbs = Binary.encode x

verifySignedBlob :: Binary a => ByteString -> SignedBlob a -> Either String a
verifySignedBlob secret (SignedBlob lbs h)
    | h == h'   = Right (Binary.decode lbs)
    | otherwise = Left $ "HMACs don't match: " ++ show (Base16.encode h) ++ " /= " ++ show (Base16.encode h')
  where
    h' = SHA512.hmaclazy secret lbs

-------------------------------------------------------------------------------
-- Email
-------------------------------------------------------------------------------

data EarlyCaringEmail = EarlyCaringEmail
    { emailAddress :: !Email
    , emailSubject :: !Text
    , emailBody    :: !LazyText
    }
  deriving (Show, Generic)

instance Binary EarlyCaringEmail

instance ToHtml EarlyCaringEmail where
    toHtmlRaw = toHtml
    toHtml (EarlyCaringEmail a s b) = pre_ $ do
        "To:      " <> toHtml a <> "\n"
        "Subject: " <> toHtml s <> "\n\n"
        toHtml b

-------------------------------------------------------------------------------
-- MonthFlex
-------------------------------------------------------------------------------

data MonthFlex = MonthFlex
    { mfMarkedHours :: !(NDT 'Hours Deci)
    , mfCapacity    :: !(NDT 'Hours Deci)
    }
  deriving Show

instance Semigroup MonthFlex where
    MonthFlex x y <> MonthFlex x' y' = MonthFlex (x + x') (y + y')

instance Monoid MonthFlex where
    mempty = MonthFlex 0 0
    mappend = (<>)

-------------------------------------------------------------------------------
-- Balance
-------------------------------------------------------------------------------

data Balance = Balance
    { balanceEmployee     :: P.Employee
    , balanceSupervisor   :: Maybe P.Employee
    , balanceHours        :: !(NDT 'Hours Deci)
    , balanceMissingHours :: !(NDT 'Hours Deci)
    , balanceAbsences     :: [Interval Day]
    , balanceMonthFlex    :: !(Map Month MonthFlex)
    }

isPermanentAllIn :: Balance -> Bool
isPermanentAllIn b =
    Just P.PermanentAllIn == balanceEmployee b ^. P.employeeContractType

balanceNormal :: Day -> Balance -> Bool
balanceNormal = balanceNormalFlex /\ balanceNormalMonthFlex /\ balanceNormalAbsences /\ balanceNormalAbsenceDays

balanceNormalFlex :: Day -> Balance -> Bool
balanceNormalFlex _ Balance {balanceHours = h, balanceMissingHours = m }
    = negate 20 <= t && t <= 40
  where
    t = h + m

balanceNormalMonthFlex :: Day -> Balance -> Bool
balanceNormalMonthFlex _ Balance { balanceMonthFlex = m } =
    all ok ms
  where
    ms = take 2 $ Map.elems m

    ok (MonthFlex marked capa)
        = negate 10 <= d && d <= 20
      where
        d = marked - capa

balanceNormalAbsences :: Day -> Balance -> Bool
balanceNormalAbsences today Balance {balanceAbsences = a } = n < 4 where
    n = countAbsences today a

balanceNormalAbsenceDays :: Day -> Balance -> Bool
balanceNormalAbsenceDays today Balance {balanceAbsences = a } = d < 20 where
    d = countAbsenceDays today a

-- | Count absences in last three months. I.e. 90 days
--
-- todo this is tricky, as we want to glue adjustent absences together!
countAbsences :: Day -> [Interval Day] -> Int
countAbsences today = length . RSet.toRangeList . RSet.fromRangeList . mapMaybe f where
    day = addDays (negate 90) today
    f a | sup a >= day = Just (inf a, sup a)
        | otherwise    = Nothing

countAbsenceDays :: Day -> [Interval Day] -> Integer
countAbsenceDays today = alaf Sum foldMap $ \a ->
    if sup a >= day
    then max 0 $ diffDays (sup a) (inf a) + 1
    else 0
  where
    day = addDays (negate 365) today
