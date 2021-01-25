{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Library.Reminder where

import Data.Maybe
import Data.Text                      (Text, intercalate)
import Data.Time
import Futurice.App.EmailProxy.Client (sendEmail)
import Futurice.App.EmailProxy.Types
import Futurice.Constants
import Futurice.IdMap                 (IdMap)
import Futurice.Prelude
import Futurice.Services
import Prelude ()
import Servant

import Futurice.App.Library.API
import Futurice.App.Library.Config
import Futurice.App.Library.Ctx
import Futurice.App.Library.Logic
import Futurice.App.Library.Markup
import Futurice.App.Library.Types

import qualified Personio as P

getAllOldLoans :: (MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> m [LoanData]
getAllOldLoans ctx = do
    today <- currentDay
    loans <- fetchAllLoans ctx
    pure $ filter (oldLoansWithDayInterval today) $ loans
  where
      oldLoansWithDayInterval today (LoanData _ loanDay _ _) = diffDays today loanDay >= 30 && diffDays today loanDay `mod` 30 == 0

reminderTemplate :: Text -> Integer -> Text -> Text
reminderTemplate loanerName days bookName = intercalate "\n"
    [ "Hello " <> loanerName <> "!"
    , "You've had the following book loaned for more than "<> textShow days <> " days"
    , "- " <> bookName
    , "Please return the book if you are not reading it anymore"
    , servicePublicUrl LibraryService <> (linkToText . fieldLink) personalLoansPageGet
    , "~Friendly Futurice Library Assistant"
    ]

sendReminderEmails :: (MonadIO m, MonadLog m, MonadBaseControl IO m, MonadCatch m) => Ctx -> IdMap P.Employee ->  m ()
sendReminderEmails ctx emps = do
    today <- currentDay
    oldLoans <- getAllOldLoans ctx
    reqs <- for oldLoans $ \l -> runMaybeT $ do
      loaner <- MaybeT $ pure $ emps ^.at (ldPersonioId l)
      loanerEmail <- MaybeT $ pure $ loaner ^. P.employeeEmail
      itemName <- do
        itemData <- MaybeT $ fetchItem ctx (ldItemId l)
        case idInfoId itemData of
          BookInfoId bookInfoid -> (fmap . fmap) (view bookTitle) (fetchBookInformation ctx bookInfoid)
          BoardGameInfoId boardGameInfoId -> (fmap . fmap) (view boardGameName) (fetchBoardGameInformation ctx boardGameInfoId)
      MaybeT $ pure $ req (loaner ^. P.employeeFullname) (diffDays today (ldDateLoaned l)) (pure $ fromEmail loanerEmail) <$> itemName
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
          , _reqSubject = "[Library] Book loan reminder"
          , _reqBody = reminderTemplate loanerId days iName
          , _reqHtmlBody = Nothing
          }
