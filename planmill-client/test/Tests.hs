{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
module Main (main) where

import Control.Monad.Memoize     (MonadMemoize (..))
import Data.Constraint
import Futurice.Constraint.Unit1 (Unit1)
import PlanMill.Internal.Prelude
import Prelude ()
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.Aeson.Compat    as Aeson
import qualified Data.Binary          as Binary
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS

import           PlanMill
import qualified PlanMill.Queries     as PMQ
import           PlanMill.Types.Query

data P a = P

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
    [ exampleResponses
    , queryHashExamples
    , queryTagAesonRoundtrip
    , queryTagBinaryRoundtrip
    , queryBinaryRoundtrip
    , responseRoundtrip
    ]

exampleResponses :: TestTree
exampleResponses = testGroup "exampleResponses" $
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

    , ex "/users/:id/vacations"
                              (P :: P Vacations)    $(embedFile "fixtures/vacations.json")
    ]
  where
    -- Example test
    ex :: forall a. (Eq a, Show a, Binary a, FromJSON a)
       => String         -- ^ Endpoint name
       -> P a            -- ^ Result type proxy
       -> BS.ByteString  -- ^ Input data
       -> TestTree
    ex name _ bs = testGroup name
        [ testProperty "FromJSON" $ once $ isRightProp x
        , testProperty "Binary roundtrip" $ once $
            Right x === binaryDecodeEither (Binary.encode x)
        ]
      where
        x :: Either String a
        x = Aeson.eitherDecodeStrict' bs :: Either String a

-------------------------------------------------------------------------------
-- Query tests
-------------------------------------------------------------------------------

queryHashExamples :: TestTree
queryHashExamples = testGroup "Query examples"
    [ testProperty "timereports Eq" $ once $ tr === tr
    ]
  where
    tr = SomeQuery (QueryTimereports (Just $ toEnum 0 ... toEnum 2) (Ident 42))

queryTagAesonRoundtrip :: TestTree
queryTagAesonRoundtrip = testGroup "QueryTag Aeson roundtrip"
    [ testProperty "I" $ forAll queryTagIdentityGen $ \sqt ->
        Right sqt === Aeson.eitherDecode (Aeson.encode sqt)
    , testProperty "Vector" $ forAll queryTagVectorGen $ \sqt ->
        Right sqt === Aeson.eitherDecode (Aeson.encode sqt)
    ]

queryTagBinaryRoundtrip :: TestTree
queryTagBinaryRoundtrip = testGroup "QueryTag Binary roundtrip"
    [ testProperty "I" $ forAll queryTagIdentityGen $ \sqt ->
        Right sqt === binaryDecodeEither (Binary.encode sqt)
    , testProperty "Vector" $ forAll queryTagVectorGen $ \sqt ->
        Right sqt === binaryDecodeEither (Binary.encode sqt)
    ]

queryBinaryRoundtrip :: TestTree
queryBinaryRoundtrip = testProperty "Query Binary roundtrip" $
    forAll queryGen $ \sq ->
        Right sq === binaryDecodeEither (Binary.encode sq)

responseRoundtrip :: TestTree
responseRoundtrip = testProperty "SomeResponse Binary roundtrip" $
    forAll someResponseGen $ \sq ->
        Right sq === binaryDecodeEither (Binary.encode sq)

-------------------------------------------------------------------------------
-- Generators
-------------------------------------------------------------------------------

queryTagIdentityGen :: Gen (SomeQueryTag I)
queryTagIdentityGen = elements
    [ SomeQueryTag QueryTagMe
    , SomeQueryTag QueryTagMeta
    , SomeQueryTag QueryTagTeam
    , SomeQueryTag QueryTagUser
    , SomeQueryTag QueryTagTimebalance
    , SomeQueryTag QueryTagTimereport
    , SomeQueryTag QueryTagTask
    , SomeQueryTag QueryTagProject
    , SomeQueryTag QueryTagAbsence
    , SomeQueryTag QueryTagAccount
    , SomeQueryTag $ QueryTagEnumDesc (Proxy :: Proxy "foo")
    , SomeQueryTag $ QueryTagEnumDesc (Proxy :: Proxy "bar")
    ]

queryTagVectorGen :: Gen (SomeQueryTag Vector)
queryTagVectorGen = elements
    [ SomeQueryTag QueryTagTeam
    , SomeQueryTag QueryTagUser
    , SomeQueryTag QueryTagTask
    , SomeQueryTag QueryTagProject
    , SomeQueryTag QueryTagAbsence
    , SomeQueryTag QueryTagAccount
    ]

queryGen :: Gen SomeQuery
queryGen = elements $ concat
    [ runPseudoPMQ PMQ.me
    , runPseudoPMQ PMQ.users
    , runPseudoPMQ $ PMQ.team (Ident 123)
    , runPseudoPMQ $ PMQ.timereports
        ($(mkDay "2016-09-01") ... $(mkDay "2016-10-01"))
        (Ident 42)
    ]

someResponseGen :: Gen SomeResponse
someResponseGen = elements [ timereportsResponse ]
  where
    uid :: UserId
    uid = Ident 42

    timereportsResponse = MkSomeResponse
        (QueryTimereports Nothing uid)
        mempty

-------------------------------------------------------------------------------
-- Monad to collect queries
-------------------------------------------------------------------------------

newtype PseudoPMQ a = PseudoPMQ { runPseudoPMQ :: [SomeQuery] }

instance Functor PseudoPMQ where
    fmap _ = PseudoPMQ . runPseudoPMQ

instance Applicative PseudoPMQ where
    pure _ = PseudoPMQ []
    PseudoPMQ a *> PseudoPMQ b = PseudoPMQ (a <> b)
    PseudoPMQ a <*> PseudoPMQ b = PseudoPMQ (a <> b)

instance Monad PseudoPMQ where
    return = pure
    (>>)   = (*>)

    -- Note: we stop at the first bind
    -- This makes this not obey the laws
    --
    -- So this is a monad in a sense `Const ()` is.
    PseudoPMQ a >>= _ = PseudoPMQ a

instance MonadPlanMillConstraint PseudoPMQ where
    type MonadPlanMillC PseudoPMQ = Unit1

    entailMonadPlanMillCVector _ _ = Sub Dict

instance MonadPlanMillQuery PseudoPMQ where
    planmillQuery q = PseudoPMQ [SomeQuery q]

-- Do not memoize
instance MonadMemoize PseudoPMQ where memo _  = id

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

isRightProp :: (Show a) => Either a b -> Property
isRightProp (Right _) = property True
isRightProp (Left  e) =
    counterexample ("got: Left $ " ++ show e) False

binaryDecodeEither :: Binary a => LBS.ByteString -> Either String a
binaryDecodeEither lbs = case Binary.decodeOrFail lbs of
    Left (_, _, err) -> Left err
    Right (_, _, x)  -> Right x
