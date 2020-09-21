{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Binary.Tagged
import Futurice.Prelude
import GHC.TypeLits          (natVal)
import Prelude ()
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy   as LBS
import qualified Futurice.GitHub        as GH
import qualified PlanMill.Types         as PM
import qualified PlanMill.Types.Query   as PM

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
        h === LBS.fromStrict (Base16.encode (structuralInfoSha1Digest (structuralInfo p)))

    versionProxy :: Proxy a -> Proxy (SemanticVersion a)
    versionProxy _ = Proxy

    tags :: [BTTest]
    tags =
        [ BTTest "PlanMill.SomeResponse" (Proxy :: Proxy PM.SomeResponse)
            0 "f779754a9cc1c8fa326e66633b88807f3e14a1c7"
        , BTTest "planmill-haxl endpoint" (Proxy :: Proxy [Either Text PM.SomeResponse])
            0 "2cc1e6f4c0dd4dc2b159ea0c58c247ca7ade72d4"
        , BTTest "PlanMill.Projects" (Proxy :: Proxy PM.Projects)
            0 "943eec14806b8d90fa7d3e4566f4aca06e8c1b2d"
        , BTTest "PlanMill.Tasks" (Proxy :: Proxy PM.Tasks)
            0 "c0af841c9b60ca6217cd9cfb05eaa568ef3b89b1"
        , BTTest "PlanMill.CapacityCalendars" (Proxy :: Proxy PM.CapacityCalendars)
            0 "1fde910b8a5fc395de41cf0cda34f17fe9d141cd"
        , BTTest "GitHub.SomeResponse" (Proxy :: Proxy GH.SomeResponse)
            0 "b1ea46b81cd6e0caa2fc5b4d0fafef0336de5da5"
        ]
