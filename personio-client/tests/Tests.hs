{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
import Data.Aeson.Compat
import Data.Aeson.Types      (parseEither)
import Data.FileEmbed
import Futurice.Prelude
import Prelude ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Personio

main :: IO ()
main = defaultMain $ testGroup "tests"
    [ properties
    , examples
    ]

-------------------------------------------------------------------------------
-- Properties
-------------------------------------------------------------------------------

properties :: TestTree
properties = testGroup "QuickCheck"
    [ testProperty "Employee roundtrip" employeeAesonRoundtrip
    ]

employeeAesonRoundtrip :: Employee -> Property
employeeAesonRoundtrip e = lhs === rhs
  where
    lhs = eitherDecode (encode e)
    rhs = Right e

-------------------------------------------------------------------------------
-- Examples
-------------------------------------------------------------------------------

examples :: TestTree
examples = testGroup "HUnit"
    [ testCase "parsePersonioEmployee" $ do
        contents <- contentsM
        e <- either fail pure $ parseEither parsePersonioEmployee contents
        "Teemu" @=? e ^. employeeFirst
        "Teekkari" @=? e ^. employeeLast
        Just $(mkDay "2017-05-29") @=? e ^. employeeHireDate
        Nothing @=? e ^. employeeEndDate
        "Developer (Primary)" @=? e ^. employeeRole
        "teemu.teekkari@example.com" @=? e ^. employeeEmail
        "+123 5678910" @=? e ^. employeePhone
        Just (EmployeeId 1337) @=? e ^. employeeSupervisorId
        Just "A Tribe" @=? e ^. employeeTribe
        Just "Helsinki" @=? e ^. employeeOffice
        Just "gitMastur" @=? e ^. employeeGithub
    , testCase "validatePersonioEmployee validates GitHub" $ do
        contents <-  decodeStrict $(makeRelativeToProject "fixtures/employee-i-github.json" >>= embedFile)
        ev <- either fail pure $ parseEither validatePersonioEmployee contents
        assertBool (show ev) $
            GithubInvalid "http://github.com/gitMastur" `elem` ev ^. evMessages
    ]
  where
    contentsM = decodeStrict $(makeRelativeToProject "fixtures/employee.json" >>= embedFile)
