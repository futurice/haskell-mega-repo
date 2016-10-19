{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main (main) where

import Data.Aeson.Compat     (FromJSON, eitherDecodeStrict')
import Data.FileEmbed        (embedFile)
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.ByteString as BS

import PlanMill
import PlanMill.Types.Query
import Numeric.Interval.NonEmpty ((...))

data P a = P

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
    [ exampleResponses
    , queryHashExamples
    ]

exampleResponses :: TestTree
exampleResponses = testGroup "exampleResponses"
    [ ex "/me"                (P :: P Me)           $(embedFile "fixtures/me.json")
    , ex "/timereports"       (P :: P Timereports)  $(embedFile "fixtures/timereports.json")
    , ex "/timereports/:id"   (P :: P Timereport)   $(embedFile "fixtures/timereport.json")
    , ex "/absences"          (P :: P Absences)     $(embedFile "fixtures/absences.json")
    , ex "/absences/:id"      (P :: P Absence)      $(embedFile "fixtures/absence.json")
    , ex "/project"           (P :: P Project)      $(embedFile "fixtures/project.json")
    , ex "/projects"          (P :: P Projects)     $(embedFile "fixtures/projects.json")
    , ex "/projects/:id/assignments"
                              (P :: P Assignments)  $(embedFile "fixtures/assignments.json")
    , ex "/accounts"          (P :: P Accounts)     $(embedFile "fixtures/accounts.json")
    , ex "/actions"           (P :: P Actions)      $(embedFile "fixtures/actions.json")
    , ex "/capacitycalendars" (P :: P CapacityCalendars)
                                                    $(embedFile "fixtures/capacitycalendars.json")
    , ex "/contacts"          (P :: P Contacts)     $(embedFile "fixtures/contacts.json")
    , ex "/tasks/:id"         (P :: P Task)         $(embedFile "fixtures/task.json")
    , ex "/users"             (P :: P Users)        $(embedFile "fixtures/users.json")
    , ex "/users/:id"         (P :: P User)         $(embedFile "fixtures/user.json")
    , ex "/users/:id/reportableassignments"
                              (P :: P ReportableAssignments)
                                                    $(embedFile "fixtures/reportableassignments.json")
    , ex "/users/:id/timebalance"
                              (P :: P TimeBalance)  $(embedFile "fixtures/timebalance.json")
    , ex "/users/:id/capacity"
                              (P :: P UserCapacities)
                                                    $(embedFile "fixtures/usercapacity.json")
    , ex "/teams"             (P :: P Teams)        $(embedFile "fixtures/teams.json")
    , ex "/teams/:id"         (P :: P Team)         $(embedFile "fixtures/team.json")
    ]
  where
    -- Example test
    ex :: forall a. FromJSON a
       => String         -- ^ Endpoint name
       -> P a            -- ^ Result type proxy
       -> BS.ByteString  -- ^ Input data
       -> TestTree
    ex name _ bs = testProperty name
                 . once
                 . isRightProp
                 $ (eitherDecodeStrict' bs :: Either String a)

queryHashExamples :: TestTree
queryHashExamples = testGroup "Query examples"
    [ testProperty "timereports Eq" $ once $ tr === tr
    ]
  where
    tr = SomeQuery (QueryTimereports (Just $ toEnum 0 ... toEnum 2) (Ident 42))

isRightProp :: (Show a) => Either a b -> Property
isRightProp (Right _) = property True
isRightProp (Left  e) =
    counterexample ("got: Left $ " ++ show e) False
