module Futurice.App.MegaRepoTool.GenPass where

import Futurice.CryptoRandom
import Futurice.Prelude
import Prelude ()

import Control.Monad   (replicateM, replicateM_)
import Data.ByteString (pack)

import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.Text                  as T

cmdGenPass :: IO ()
cmdGenPass = generatePassword 30
  where
    generatePassword :: Int -> IO ()
    generatePassword l = do
        g <- mkCryptoGen
        void $ runCRandTThrow' g $ replicateM_ 16 $ do
            p <- generatePassword' l
            lift $ putStrLn p

    generatePassword' :: (Monad m) => Int -> CRandT CryptoGen CryptoGenError m String
    generatePassword' l = do
        T.unpack . decodeUtf8Lenient . Base64.encode . pack <$> replicateM l getCRandom
