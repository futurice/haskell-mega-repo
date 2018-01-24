module Futurice.App.MegaRepoTool.GenPass where

import Futurice.Prelude
import Prelude ()
import Futurice.CryptoRandom

import Control.Monad (replicateM, replicateM_)

cmdGenPass :: IO ()
cmdGenPass = generatePassword 32
  where
    chars :: String
    chars = -- ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "!@#?-_,."
        ".-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

    generatePassword :: Int -> IO ()
    generatePassword l = do
        g <- mkCryptoGen
        void $ runCRandTThrow' g $ replicateM_ 16 $ do
            p <- generatePassword' l
            lift $ putStrLn p

    generatePassword' :: Int -> CRandT CryptoGen CryptoGenError IO String
    generatePassword' l = replicateM l (element chars)

    element :: MonadCRandomR e m => [a] -> m a
    element list = (list !!) <$> getCRandomR (0, length list - 1)
