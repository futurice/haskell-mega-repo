module Eldapo (defaultMain) where

import Network.Arcola (Connection, connLazyRead, connLazySendAll, run)

import qualified Data.Attoparsec.ByteString      as A
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString.Lazy            as LBS

import Neleus

defaultMain :: IO ()
defaultMain = do
    putStrLn "Start"
    run 9999 $ \conn -> do
        putStrLn "Connected"
        lbs <- connLazyRead conn
        let s = parseStream asn1P lbs
        loop conn 1 s
  where
    loop :: Connection -> Integer -> Stream String ASN1 -> IO ()
    loop conn i s = case s of
        Done       -> print s
        Fail e     -> print e
        Yield a s' -> do
            print a
            connLazySendAll conn (encode (searchResDone i))
            loop conn (succ i) s'


-- https://lapo.it/asn1js/#303E020101633904000A01000A0100020100020100010100870B6F626A656374636C61737330190417737570706F727465645341534C4D656368616E69736D73

{-
SEQUENCE(2 elem)
  INTEGER1
  Application 3(8 elem)
    OCTET STRING(0 elem)
    ENUMERATED
    ENUMERATED
    INTEGER0
    INTEGER0
    BOOLEANfalse
    [7]objectclass
    SEQUENCE(1 elem)
      OCTET STRINGsupportedSASLMechanisms
-}

-------------------------------------------------------------------------------
-- Stream
-------------------------------------------------------------------------------

parseStream :: A.Parser a -> LBS.ByteString -> Stream String a
parseStream p = go where
    go lbs
        | LBS.null lbs = Done
        | otherwise = case AL.parse p lbs of
            AL.Fail _ _ err -> Fail err
            AL.Done lbs' x  -> Yield x (go lbs')

data Stream e a
    = Yield a (Stream e a)
    | Fail e
    | Done
  deriving Show

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------
