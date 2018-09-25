{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Either
import Data.IORef
import Database.PostgreSQL.Simple
import Futurice.App.Sisosota.Types
import Futurice.Postgres
import Futurice.Prelude
import Log.Data
import Log.Logger
import Prelude ()
import Test.Tasty
import Test.Tasty.HUnit

import Futurice.App.Library.Logic
import Futurice.App.Library.Types
import Futurice.App.Library.Types.BoardGameInformation
       (BoardGameInformationId (..))
import Futurice.App.Library.Types.BookInformation      (BookInformationId (..))

import qualified Data.Text as T

main :: IO ()
main = defaultMain tests

-- TODO: how to change this take the connection details from env like the app
testConnection :: (MonadIO m) => m (Pool Connection)
testConnection = createPostgresPool defaultConnectInfo

assertNoAttentionLogMessages :: Text -> IO ()
assertNoAttentionLogMessages "" = pure ()
assertNoAttentionLogMessages err = assertFailure (T.unpack err)

withSimpleAttentionLogger :: (Logger -> IO r) -> IO Text
withSimpleAttentionLogger act = do
    ref <- newIORef ""
    logger <- mkLogger "" (\logMessage -> case lmLevel logMessage of
                              LogAttention -> modifyIORef ref (<> showLogMessage Nothing logMessage <> "\n")
                              _ -> pure ())
    _ <- act logger
    attentionMessages <- readIORef ref
    pure attentionMessages

tests :: TestTree
tests = testGroup "Sql tests"
    [ testCase "Search bookInformation" $ assertNoAttentionLogMessages =<< withSimpleAttentionLogger (\logger -> do
          pp <- runLogT "testing" logger testConnection
          for_ allBookSortCriteriaAndStart $ \cri -> for allDirections $ \dir ->
            runLogT "testing" logger $ fetchInformationsWithCriteria pp cri dir testLimit Nothing :: IO [BookInformation])
    , testCase "Search bookInformation with search" $ assertNoAttentionLogMessages =<< withSimpleAttentionLogger (\logger -> do
          pp <- runLogT "testing" logger testConnection
          for_ allBookSortCriteriaAndStart $ \cri -> for allDirections $ \dir ->
            runLogT "testing" logger $ fetchInformationsWithCriteria pp cri dir testLimit (Just "testing") :: IO [BookInformation])
    , testCase "Search boardgameInformation" $ assertNoAttentionLogMessages =<< withSimpleAttentionLogger (\logger -> do
          pp <- runLogT "testing" logger testConnection
          for_ allBoardGameSortCriteriaAndStart $ \cri -> for allDirections $ \dir ->
            runLogT "testing" logger $ fetchInformationsWithCriteria pp cri dir testLimit Nothing :: IO [BoardGameInformation])
    , testCase "Search boardgameInformation with search" $ assertNoAttentionLogMessages =<< withSimpleAttentionLogger (\logger -> do
          pp <- runLogT "testing" logger testConnection
          for_ allBoardGameSortCriteriaAndStart $ \cri -> for allDirections $ \dir ->
            runLogT "testing" logger $ fetchInformationsWithCriteria pp cri dir testLimit (Just "testing") :: IO [BoardGameInformation])
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
