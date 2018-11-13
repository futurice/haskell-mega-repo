{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Library.Remainder where

import Data.Maybe
import Data.Text                      (Text, intercalate, pack)
import Data.Time
import Futurice.App.EmailProxy.Client (sendEmail)
import Futurice.App.EmailProxy.Types
import Futurice.IdMap                 (IdMap)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Library.Config
import Futurice.App.Library.Ctx
import Futurice.App.Library.Logic
import Futurice.App.Library.Types

import qualified Personio as P

getAllOldLoans :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> m [LoanData]
getAllOldLoans ctx = do
    now <- currentDay
    loans <- runLogT "loan-remainder" (ctxLogger ctx) $ fetchAllLoans (ctxPostgres ctx)
    pure $ filter (oldLoansWithDayInterval now) $ loans
  where
      oldLoansWithDayInterval n (LoanData _ loanDay _ _) = diffDays n loanDay >= 30 && diffDays n loanDay `mod` 30 == 0

remainderTemplate :: Text -> Integer -> Text -> Text
remainderTemplate loanerName days bookName = intercalate "\n"
    ["Hello " <> loanerName <> "!"
    , "You've had the following book loaned for more than "<> pack (show days) <> " days"
    , "- " <> bookName
    , "Please return the book if you are not reading it anymore"
    , "https://library.app.futurice.com/user/page"
    , "~Friendly Futurice Library Assistant"]

sendRemainderEmails :: (MonadIO m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> IdMap P.Employee ->  m ()
sendRemainderEmails ctx emps = do
    now <- currentDay
    oldLoans <- getAllOldLoans ctx
    reqs <- for oldLoans $ \l -> runMaybeT $ do
      loaner <- MaybeT $ pure $ emps ^.at (ldPersonioId l)
      loanerEmail <- MaybeT $ pure $ loaner ^. P.employeeEmail
      itemName <- do
        itemData <- MaybeT $ fetchItem (ctxPostgres ctx) (ldItemId l)
        case idInfoId itemData of
          BookInfoId bookInfoid -> (fmap . fmap) (^. bookTitle) (fetchBookInformation (ctxPostgres ctx) bookInfoid)
          BoardGameInfoId boardGameInfoId -> (fmap . fmap) (^. boardGameName) (fetchBoardGameInformation (ctxPostgres ctx) boardGameInfoId)
      MaybeT $ pure $ req (loaner ^. P.employeeFullname) (diffDays now (ldDateLoaned l)) (pure $ fromEmail loanerEmail) <$> itemName
    void $ liftIO $ for (catMaybes reqs) $ \r -> do
        x <- tryDeep $ sendEmail manager emailProxyUrl r
        case x of
          Left exc -> runLogT "loan-remainder" (ctxLogger ctx) $ void $ logAttention "sendEmail failed" (show exc)
          Right () -> return ()
  where
      emailProxyUrl = (cfgEmailProxyUrl . ctxConfig) ctx
      manager = ctxManager ctx
      req loanerId days loanerEmail iName = Req
          { _reqTo = loanerEmail
          , _reqCc = Nothing
          , _reqBcc = Nothing
          , _reqReplyTo = Nothing
          , _reqSubject = "LIBRARY: Reminder"
          , _reqBody = remainderTemplate loanerId days iName
          }
