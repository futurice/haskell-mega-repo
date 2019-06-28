{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Binary.Tagged
import Futurice.Prelude
import GHC.TypeLits          (natVal)
import Prelude ()
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.ByteString.Base16.Lazy as Base16
import qualified Futurice.GitHub             as GH
import qualified PlanMill.Types              as PM
import qualified PlanMill.Types.Query        as PM

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "proxy-app"
    [ binaryTagTests
    ]

data BTTest where
    BTTest
        :: (HasStructuralInfo a, HasSemanticVersion a)
        => String -> Proxy a -> Int -> LazyByteString -> BTTest

binaryTagTests :: TestTree
binaryTagTests = testGroup "BinaryTagged tags" $ map mk tags
  where
    mk :: BTTest -> TestTree
    mk (BTTest name p ver h) = testProperty name $ once $
        ver === fromIntegral (natVal (versionProxy p)) .&&.
        h === Base16.encode (structuralInfoSha1ByteStringDigest (structuralInfo p))

    versionProxy :: Proxy a -> Proxy (SemanticVersion a)
    versionProxy _ = Proxy

    tags :: [BTTest]
    tags =
        [ BTTest "PlanMill.SomeResponse" (Proxy :: Proxy PM.SomeResponse)
            0 "a63534ce5176ce356f996bb34b63f2b5b781ad08"
        , BTTest "planmill-haxl endpoint" (Proxy :: Proxy [Either Text PM.SomeResponse])
            0 "14f6d4cbe4d7278e4f9f24667f08104b05ff061b"
        , BTTest "PlanMill.Projects" (Proxy :: Proxy PM.Projects)
            0 "6721479d302ee1ebe18b7eabd4bffc6e6515b853"
        , BTTest "PlanMill.Tasks" (Proxy :: Proxy PM.Tasks)
            0 "c0af841c9b60ca6217cd9cfb05eaa568ef3b89b1"
        , BTTest "PlanMill.CapacityCalendars" (Proxy :: Proxy PM.CapacityCalendars)
            0 "1fde910b8a5fc395de41cf0cda34f17fe9d141cd"
        , BTTest "GitHub.SomeResponse" (Proxy :: Proxy GH.SomeResponse)
            0 "8a6e66865d0dd5f2da6d4d63389b4b24d01b1ae3"
        ]
