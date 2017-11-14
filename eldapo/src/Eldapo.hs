module Eldapo (defaultMain) where

import Network.Arcola (Connection, connLazyRead, connLazySendAll, run)

import qualified Data.ByteString.Lazy            as LBS

import Neleus
import qualified Neleus.Parser as P

defaultMain :: IO ()
defaultMain = do
    putStrLn "Start"
    run 9999 $ \conn -> do
        putStrLn "Connected"
        lbs <- connLazyRead conn
        loop conn 1 lbs
  where
    loop :: Connection -> Integer -> LBS.ByteString -> IO ()
    loop conn i lbs
        | LBS.null lbs = print "DONE"
        | otherwise = case P.decode stepDER P.asn1Parser lbs of
            Left err        -> do
                print "ERROR"
                print err
            Right (x, lbs') -> do
                print x
                connLazySendAll conn (encode (searchResDone i))
                loop conn (succ i) lbs'
