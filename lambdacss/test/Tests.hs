{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Control.Monad       (unless)
import Data.TreeDiff       (ToExpr, ansiWlEditExpr, ediff)
import System.Console.ANSI (setSGRCode)
import System.FilePath     (takeFileName, (</>))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS

import LambdaCSS

main :: IO ()
main = defaultMain $ testGroup "fixtures"
    [ fixture "select2.min.css"
    , fixture "jquery-ui.min.css"
    , fixture "foundation.min.css"
    , testGroup "bootstrap-3.3.7"
        [ fixture $ "bootstrap-3.3.7" </> "bootstrap-theme.css"
        , fixture $ "bootstrap-3.3.7" </> "bootstrap-theme.min.css"
        , fixture $ "bootstrap-3.3.7" </> "bootstrap.css"
        , fixture $ "bootstrap-3.3.7" </> "bootstrap.min.css"
        ]
    , testGroup "bootstrap-4.0.0"
        [ fixture $ "bootstrap-4.0.0" </> "bootstrap-reboot.css"
        , fixture $ "bootstrap-4.0.0" </> "bootstrap-reboot.min.css"
        , fixture $ "bootstrap-4.0.0" </> "bootstrap-grid.css"
        , fixture $ "bootstrap-4.0.0" </> "bootstrap-grid.min.css"
        -- TODO: need supports
        -- , fixture $ "bootstrap-4.0.0" </> "bootstrap.css"
        -- , fixture $ "bootstrap-4.0.0" </> "bootstrap.min.css"
        ]
    ]

fixture :: FilePath -> TestTree
fixture n = testCase (takeFileName n) $ do
    contents <- BS.readFile $ "fixtures" </> n
    case parseLambdaCSS contents of
        Left e  -> assertFailure (setSGRCode [] ++ e)
        Right x -> do
            let bs = LBS.toStrict $ printLambdaCSS x
            case parseLambdaCSS bs of
                Left e  -> assertFailure e
                Right y -> unless (x == y) $ assertFailure $
                    setSGRCode [] ++ show (ansiWlEditExpr $ ediff x y)

-------------------------------------------------------------------------------
-- Orphans
-------------------------------------------------------------------------------

instance ToExpr Any
instance ToExpr Declaration
instance ToExpr KeyframeBlock
instance ToExpr KeyframeSelector
instance ToExpr NestedStatement
instance ToExpr Ruleset
instance ToExpr Selector
instance ToExpr SelectorMod
instance ToExpr AttributeOp
instance ToExpr SimpleSelector
instance ToExpr Statement
instance ToExpr Stylesheet
