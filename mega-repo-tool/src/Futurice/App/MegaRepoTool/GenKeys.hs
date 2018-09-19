{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.MegaRepoTool.GenKeys where

import Futurice.Prelude
import Prelude ()

import qualified Crypto.Sign.Ed25519    as Ed
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text.IO           as T

cmdGenKeys :: IO ()
cmdGenKeys = do
    (Ed.PublicKey pk', Ed.SecretKey sk') <- Ed.createKeypair
    let pk = Base16.encode pk'
    let sk = Base16.encode sk'

    T.putStrLn $ "Public: " <> decodeUtf8Lenient pk
    T.putStrLn $ "Secret: " <> decodeUtf8Lenient sk
