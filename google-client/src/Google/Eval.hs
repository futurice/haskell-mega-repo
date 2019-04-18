{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Google.Eval where

import Futurice.Prelude
import Network.Google
import Network.Google.AppsCalendar hiding (events)
import Network.Google.Directory
import Prelude ()
import System.IO                   (stderr)

import Google.Request
import Google.Types

import qualified Network.Google.Auth as GA

-- Think how to combine to MonadLog
-- withLogger :: Futurice.Prelude.Logger -> (LogLevel -> Builder -> IO ())
-- withLogger lgr = logger
--   where
--     logger level builder | level >= Debug = logInfo_ $ decodeUtf8Lenient . DBL.toStrict $ toLazyByteString builder
--                          | otherwise = pure ()

-- TODO: check if monadbasecontrol IO or monadIO is really needed.
-- TODO: check if there is better way to handle manager
-- TODO: simplify cfg getting


-- Used scopes has to be exactly same as the scope privileges that the credentials have
-- i.e you can't use readonly scope with write premitted scope

type GoogleConstrains m env = (MonadIO m,MonadCatch m, MonadBaseControl IO m, MonadReader env m, HasGoogleCfg env, HasHttpManager env)

evalGoogleReq :: GoogleConstrains m env => Req a -> m a
evalGoogleReq (ReqCalendarResources ReadOnly) = do
    cfg <- view googleCfg
    mgr <- view httpManager
    let cred = GA.serviceAccountUser (Just $ serviceAccountUser cfg) $ toCredentials cfg
    lgr <- newLogger Error stderr
    env <- newEnvWith cred lgr mgr <&> (envScopes .~ adminDirectoryResourceCalendarReadOnlyScope)
    liftIO $ runResourceT $ runGoogle env $ do
        x <- send $ resourcesCalendarsList "my_customer"
        pure $ x ^. crsItems
evalGoogleReq (ReqCalendarResources AlsoWriteAccess) = do
    cfg <- view googleCfg
    mgr <- view httpManager
    let cred = GA.serviceAccountUser (Just $ serviceAccountUser cfg) $ toCredentials cfg
    lgr <- newLogger Error stderr
    env <- newEnvWith cred lgr mgr <&> (envScopes .~ adminDirectoryResourceCalendarScope)
    liftIO $ runResourceT $ runGoogle env $ do
        x <- send $ resourcesCalendarsList "my_customer"
        pure $ x ^. crsItems
evalGoogleReq (ReqEvents ReadOnly startDay endDay email) = do
    cfg <- view googleCfg
    mgr <- view httpManager
    let cred = GA.serviceAccountUser (Just $ serviceAccountUser cfg) $ toCredentials cfg
    let roomReservationNotCancelled e = not $ any (\a -> a ^.  eaEmail == Just email && a ^. eaResponseStatus == Just "declined" ) $ e ^. eAttendees
    lgr <- newLogger Error stderr
    env <- newEnvWith cred lgr mgr <&> (envScopes .~ calendarReadOnlyScope)
    events <- liftIO $ runResourceT $ runGoogle env $ do
        eventList <- send (eventsList email
                           & elTimeMin .~ Just (UTCTime startDay 0) -- TODO: maybe take UTCTime?
                           & elTimeMax .~ Just (UTCTime endDay 0)
                           & elSingleEvents .~ Just True)
        pure $ eventList ^. eveItems
    pure $ filter (\e -> e ^. eStatus /= Just "cancelled" && roomReservationNotCancelled e) events
evalGoogleReq (ReqEvents AlsoWriteAccess startDay endDay email) = do
    cfg <- view googleCfg
    mgr <- view httpManager
    let cred = GA.serviceAccountUser (Just $ serviceAccountUser cfg) $ toCredentials cfg
    let roomReservationNotCancelled e = not $ any (\a -> a ^.  eaEmail == Just email && a ^. eaResponseStatus == Just "declined" ) $ e ^. eAttendees
    lgr <- newLogger Error stderr
    env <- newEnvWith cred lgr mgr <&> (envScopes .~ calendarScope)
    events <- liftIO $ runResourceT $ runGoogle env $ do
        eventList <- send (eventsList email
                           & elTimeMin .~ Just (UTCTime startDay 0) -- TODO: maybe take UTCTime?
                           & elTimeMax .~ Just (UTCTime endDay 0)
                           & elSingleEvents .~ Just True)
        pure $ eventList ^. eveItems
    pure $ filter (\e -> e ^. eStatus /= Just "cancelled" && roomReservationNotCancelled e) events
evalGoogleReq (ReqInvite calendarEvent) = do
    cfg <- view googleCfg
    mgr <- view httpManager
    let cred = GA.serviceAccountUser (Just $ serviceAccountUser cfg) $ toCredentials cfg
    let eventAttendees = fmap (\a -> eventAttendee & eaEmail .~ Just a) (calendarEvent ^. ceAttendees)
    let eventStart = eventDateTime & edtDateTime .~ Just (calendarEvent ^. ceStartTime)
    let eventEnd   = eventDateTime & edtDateTime .~ Just (calendarEvent ^. ceEndTime)
    lgr <- newLogger Error stderr
    env <- newEnvWith cred lgr mgr <&> (envScopes .~ calendarEventsScope)
    liftIO $ runResourceT $ runGoogle env $
        send (eventsInsert "primary" (event & eAttendees   .~ eventAttendees
                                            & eStart       .~ Just eventStart
                                            & eEnd         .~ Just eventEnd
                                            & eSummary     .~ Just (calendarEvent ^. ceSummary)
                                            & eDescription .~ Just (calendarEvent ^. ceDescription)))
evalGoogleReq (ReqDeleteEvent eventId) = do
    cfg <- view googleCfg
    mgr <- view httpManager
    let cred = GA.serviceAccountUser (Just $ serviceAccountUser cfg) $ toCredentials cfg
    lgr <- newLogger Error stderr
    env <- newEnvWith cred lgr mgr <&> (envScopes .~ calendarScope)
    void $ liftIO $ runResourceT $ runGoogle env $ do
        void $ send (eventsDelete "primary" eventId)
evalGoogleReq (ReqPatchEvent eventId calendarEvent) = do
    cfg <- view googleCfg
    mgr <- view httpManager
    let cred = GA.serviceAccountUser (Just $ serviceAccountUser cfg) $ toCredentials cfg
    let eventAttendees = fmap (\a -> eventAttendee & eaEmail .~ Just a) (calendarEvent ^. ceAttendees)
    lgr <- newLogger Error stderr
    env <- newEnvWith cred lgr mgr <&> (envScopes .~ calendarScope)
    liftIO $ runResourceT $ runGoogle env $ do
        send (eventsPatch "primary" (event & eAttendees .~ eventAttendees) eventId)

evalGoogleReqIO :: GoogleCredentials -> Manager -> Req a -> IO a
evalGoogleReqIO cred mgr req = flip runReaderT (Cfg cred mgr) $ evalGoogleReq req
