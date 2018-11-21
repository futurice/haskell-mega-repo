{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent.STM
import Data.Either
import Database.PostgreSQL.Simple
import Futurice.App.Sisosota.Types
import Futurice.EnvConfig
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

testConnection :: (MonadIO m, MonadLog m) => m (Pool Connection)
testConnection = do
    connectInfo <- getConfig' "TEST" envConnectInfo
    createPostgresPool connectInfo

assertNoAttentionLogMessages :: LogT IO () -> IO ()
assertNoAttentionLogMessages test = do
    logMessages <- withSimpleAttentionLogger (\logger -> runLogT "testing" logger test)
    case logMessages of
      [] -> pure ()
      xs -> assertFailure (T.unpack $ T.intercalate "\n" ((showLogMessage Nothing) <$> xs))

withSimpleAttentionLogger :: (Logger -> IO r) -> IO [LogMessage]
withSimpleAttentionLogger act = do
    var <- newTVarIO []
    logger <- mkLogger "" (\logMessage -> case lmLevel logMessage of
                              LogAttention -> atomically $ modifyTVar' var (<> [logMessage])
                              _ -> pure ())
    _ <- act logger
    attentionMessages <- readTVarIO var
    pure attentionMessages

tests :: TestTree
tests = testGroup "Sql tests"
    [ testCase "Search bookInformation" $ assertNoAttentionLogMessages $
        testConnection >>= \pp ->
        for_ allBookCriterias $ \cri ->
        for_ allBookItems $ \mii ->
        for_ allDirections $ \dir ->
        for_ allTestLibraries $ \lib ->
        for_ allAvailibilities $ \availibility ->
            fetchInformationsWithCriteria pp (BookSort cri) mii dir testLimit Nothing lib availibility
    , testCase "Search bookInformation with search" $ assertNoAttentionLogMessages $
        testConnection >>= \pp ->
        for_ allBookCriterias $ \cri ->
        for_ allBookItems $ \mii ->
        for_ allDirections $ \dir ->
        for_ allTestLibraries $ \lib ->
        for_ allAvailibilities $ \availibility ->
            fetchInformationsWithCriteria pp (BookSort cri) mii dir testLimit (Just "testing") lib availibility
    , testCase "Search boardgameInformation" $ assertNoAttentionLogMessages $
        testConnection >>= \pp ->
        for_ allBoardGameCriterias $ \cri ->
        for_ allBoardGameItems $ \mii ->
        for_ allDirections $ \dir ->
        for_ allTestLibraries $ \lib ->
        for_ allAvailibilities $ \availibility ->
            fetchInformationsWithCriteria pp (BoardGameSort cri) mii dir testLimit Nothing lib availibility
    , testCase "Search boardgameInformation with search" $ assertNoAttentionLogMessages $
        testConnection >>= \pp ->
        for_ allBoardGameCriterias $ \cri ->
        for_ allBoardGameItems $ \mii ->
        for_ allDirections $ \dir ->
        for_ allTestLibraries $ \lib ->
        for_ allAvailibilities $ \availibility ->
            fetchInformationsWithCriteria pp (BoardGameSort cri) mii dir testLimit (Just "testing") lib availibility
    ]
  where
    testContentHash          = head $ rights $ [contentHashFromText "DRmKYxSH8aggYWd7S5ZNIKPdcPRho6Taxx5BhHRC0cr-0B7OGttEi_mq6dr4JP1r_aVHg3SU-d6PTWuPx2SCkw=="] -- Partial!

    allBookCriterias         = [minBound .. maxBound] :: [BookSortCriteria]
    allBoardGameCriterias    = [minBound .. maxBound] :: [BoardGameSortCriteria]
    allDirections            = [minBound .. maxBound] :: [SortDirection]

    testBookInformation      = BookInformation (BookInformationId 2) "TestTitle" "1234567890" "TestAuthor" "TestPublisher" 1963 testContentHash "Testlink"
    testBoardGameInformation = BoardGameInformation (BoardGameInformationId 2) "TestName" Nothing Nothing Nothing Nothing

    allBookItems             = [ Nothing, Just (ItemBook testBookInformation) ]
    allBoardGameItems        = [ Nothing, Just (ItemBoardGame testBoardGameInformation) ]

    allTestLibraries         = AllLibraries : fmap JustLibrary allLibraries
    allAvailibilities        = [False, True]
    testLimit                = 20
