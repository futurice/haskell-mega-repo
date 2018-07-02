{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.FUM.Types.Password where

import Control.Monad             (replicateM)
import Data.Char                 (ord)
import Data.Time                 (addDays)
import Futurice.CryptoRandom
import Futurice.Generics
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Linear                    (V4 (..))
import Prelude ()
import System.POSIX.Crypt.SHA512 (cryptSHA512')

import qualified Data.ByteString as BS
import qualified Data.Text       as T
import qualified Data.Vector     as V

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data Password = Password
    { _passwordHash    :: !Text
    , _passwordExpires :: !UTCTime
    }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

makeLenses ''Password
deriveGeneric ''Password

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

deriveVia [t| ToJSON Password   `Via` Sopica Password |]
deriveVia [t| FromJSON Password `Via` Sopica Password |]

-------------------------------------------------------------------------------
-- Html
-------------------------------------------------------------------------------

passwordToHtml :: Monad m => UTCTime -> Password -> HtmlT m ()
passwordToHtml now (Password h expires) = do
    if now < expires
    then do
        "Expires at "
        toHtml $ formatHumanHelsinkiTime expires
    else span_ [ class_ "alert" ] $ do
        "Expired at "
        toHtml $ formatHumanHelsinkiTime expires
    ": "
    code_ $ toHtml $ T.take 40 h <> "..."

-------------------------------------------------------------------------------
-- Policy
-------------------------------------------------------------------------------

-- | All-in-one password generation.
--
-- From FUM5:
-- An LDAP password is a combination of lowercase, uppercase, digits,
-- and special characters with characters from atleast three groups present.
makePassword
    :: (Text -> IO ())  -- ^ callback to call with cleartext password. E.g. to send SMS to the user.
    -> UTCTime          -- ^ Current time (we don't ask IO!)
    -> IO Password
makePassword callback now = do
    g <- mkCryptoGen

    (salt, idx) <- evalCRandTThrow' g $ do
        -- salt is simply 12 random bytes.
        s <- BS.pack <$> replicateM 12 getCRandom
        idx <- replicateM 12 getCRandom

        return (s, idx)

    -- Note: we can generate password which doesn't satisfy the policy
    -- (which is quite likely), but here we generate "good" passwords.
    let password = BS.pack $
            map (\i -> passwordChars' V.! (i `mod` passwordCharsLen)) idx

    callback (decodeUtf8Lenient password)

    let h = decodeUtf8Lenient $ cryptSHA512' (Just 10000) password salt

    pure Password { _passwordHash = h, _passwordExpires = addYear now }
  where
    addYear :: UTCTime -> UTCTime
    addYear (UTCTime d n) = UTCTime (addDays 365 d) n

-- | We don't include @0Oo@ or @1Ili@ characters in generated passwords.
passwordChars :: V4 (Vector Word8)
passwordChars = (fmap . fmap) (fromIntegral . ord) $ V4
    (V.fromList $ filter (`notElem` ("IO" :: String)) ['A'..'Z' ])
    (V.fromList $ filter (`notElem` ("ilo" :: String)) ['a'..'z'])
    (V.fromList "23456789")
    (V.fromList "#./+-_&\"%")

passwordChars' :: Vector Word8
passwordChars' = fold passwordChars

passwordCharsLen :: Int
passwordCharsLen = length passwordChars'
