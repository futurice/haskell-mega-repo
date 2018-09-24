{-# LANGUAGE OverloadedStrings #-}
import Data.Either
import Database.PostgreSQL.Simple
import Futurice.App.Sisosota.Types
import Futurice.EnvConfig
import Futurice.Postgres
import Futurice.Prelude
import Prelude ()
import Test.Tasty
import Test.Tasty.HUnit

import Futurice.App.Library.Logic
import Futurice.App.Library.Types
import Futurice.App.Library.Types.BoardGameInformation
       (BoardGameInformationId (..))
import Futurice.App.Library.Types.BookInformation      (BookInformationId (..))

main :: IO ()
main = defaultMain tests

-- TODO: how to change this take the connection details from env like the app
testConnection :: (MonadIO m) => m (Pool Connection)
testConnection = createPostgresPool defaultConnectInfo

tests :: TestTree
tests = testGroup "Sql tests"
    [ testCase "Search bookInformation" $ withStderrLogger $ \logger -> do
          pp <- runLogT "testing" logger testConnection
          for_ allBookSortCriteriaAndStart $ \cri -> for allDirections $ \dir ->
            runLogT "testing" logger $ fetchInformationsWithCriteria pp cri dir testLimit Nothing :: IO [BookInformation]
          pure ()
    , testCase "Search bookInformation with search" $ withStderrLogger $ \logger -> do
          pp <- runLogT "testing" logger testConnection
          for_ allBookSortCriteriaAndStart $ \cri -> for allDirections $ \dir ->
            runLogT "testing" logger $ fetchInformationsWithCriteria pp cri dir testLimit (Just "testing") :: IO [BookInformation]
          pure ()
    , testCase "Search boardgameInformation" $ withStderrLogger $ \logger -> do
          pp <- runLogT "testing" logger testConnection
          for_ allBoardGameSortCriteriaAndStart $ \cri -> for allDirections $ \dir ->
            runLogT "testing" logger $ fetchInformationsWithCriteria pp cri dir testLimit Nothing :: IO [BoardGameInformation]
          pure ()
    , testCase "Search boardgameInformation with search" $ withStderrLogger $ \logger -> do
          pp <- runLogT "testing" logger testConnection
          for_ allBoardGameSortCriteriaAndStart $ \cri -> for allDirections $ \dir ->
            runLogT "testing" logger $ fetchInformationsWithCriteria pp cri dir testLimit (Just "testing") :: IO [BoardGameInformation]
          pure ()
    ]
  where
    testContentHash = head $ rights $ [contentHashFromText "DRmKYxSH8aggYWd7S5ZNIKPdcPRho6Taxx5BhHRC0cr-0B7OGttEi_mq6dr4JP1r_aVHg3SU-d6PTWuPx2SCkw=="] -- Partial!
    testBookInformation = BookInformation (BookInformationId 2) "TestTitle" "1234567890" "TestAuthor" "TestPublisher" 1963 testContentHash "Testlink"
    testBoardGameInformation = BoardGameInformation (BoardGameInformationId 2) "TestName" Nothing Nothing Nothing Nothing
    allBookCriterias = [minBound .. maxBound] :: [BookSortCriteria]
    allBoardGameCriterias = [minBound .. maxBound] :: [BoardGameSortCriteria]
    allDirections = [minBound .. maxBound] :: [SortDirection]
    allBookSortCriteriaAndStart = concat [(flip BookCS Nothing) <$> allBookCriterias, (flip BookCS (Just testBookInformation)) <$> allBookCriterias]
    allBoardGameSortCriteriaAndStart = concat [(flip BoardGameCS Nothing) <$> allBoardGameCriterias, (flip BoardGameCS (Just testBoardGameInformation)) <$> allBoardGameCriterias]
    testLimit = 20
