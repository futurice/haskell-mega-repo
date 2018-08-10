{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Reports.MissingHoursNotifications (
    missingHoursNotifications,
    ) where

import Data.Aeson                  (object, (.=))
import Data.Time                   (defaultTimeLocale, formatTime)
import Data.Time                   (toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Futurice.Integrations
       (beginningOfPrev2Month, endOfPrevMonth, personioPlanmillMap,
       previousFriday)
import Futurice.Prelude
import Futurice.Report.Columns     (Report (..))
import Numeric.Interval.NonEmpty   ((...))
import Prelude ()

import qualified Data.HashMap.Strict            as HM
import qualified Futurice.App.EmailProxy.Client as E
import qualified Futurice.App.EmailProxy.Types  as E
import qualified Futurice.App.SmsProxy.Client   as S
import qualified Futurice.App.SmsProxy.Types    as S
import qualified Personio                       as P

import Futurice.App.Preferences.Client (getPreferences)
import Futurice.App.Preferences.Types

import Futurice.App.Reports.Config
import Futurice.App.Reports.Ctx
import Futurice.App.Reports.MissingHours
       (missingHourDay, missingHoursEmployeePredicate, missingHoursReport)
import Futurice.App.Reports.Templates

-- | Pick the latter from
-- /the previous Friday/ or
-- /the last day of the previous month/.
reportEndDate
    :: Day -- ^ "today"
    -> Day
reportEndDate day = max (previousFriday day) (endOfPrevMonth day)

checkNotificationDay :: Day -> LogT IO Text -> LogT IO Text
checkNotificationDay day m
    -- Weekend
    | wd `elem` [6, 7] = do
        logInfo "Weekend: no notifications" day
        return "ERR: Weekend"
    -- Tuesday
    | wd == 2 = m
    -- Beginning of the month
    | d >= 0 && d < 6 = m
    -- otherwise
    | otherwise = do
        logInfo "Not Tuesday and not beginning of the month" day
        return "ERR: Other"
  where
    (_, _, wd) = toWeekDate day
    (_, _, d)  = toGregorian day

missingHoursNotifications :: Ctx -> IO Text
missingHoursNotifications ctx = runLogT "missing-hours-notifications" lgr $ do
    day <- currentDay
    checkNotificationDay day $ do
        let interval = beginningOfPrev2Month day ... reportEndDate day

        (ppm, Report _ report) <- liftIO $ runIntegrations' ctx $ (,)
            <$> personioPlanmillMap
            <*> missingHoursReport missingHoursEmployeePredicate interval

        let logins = HM.keys report
        prefs <- liftIO $ getPreferences mgr preferencesBurl logins
        ifor_ report $ \login r -> do
            let pref = fromMaybe defaultPreferences $ prefs ^? ix login

            case ppm ^? ix login . _1 of
                Nothing -> logAttention "Unknown login" login
                Just p -> do
                    let humanDay :: Day -> Value
                        humanDay d = object
                            [ "day" .= formatTime defaultTimeLocale "%A %F" d
                            ]

                    let days = r ^.. _2 . folded . missingHourDay . getter humanDay

                    unless (null days) $ do
                        let params = object
                                [ "name"     .= (p ^. P.employeeFirst)
                                , "interval" .= show interval
                                , "ndays"    .= length days
                                , "hours"    .= days
                                ]

                        when (pref ^. prefHoursPingEmail) $ case p ^. P.employeeEmail of
                            Nothing -> logAttention "Employee without email" login
                            Just addr -> do
                                x <- liftIO $ tryDeep $ E.sendEmail mgr emailProxyBurl $ E.emptyReq (E.fromEmail addr)
                                    & E.reqSubject .~ "Missing hours"
                                    & E.reqBody    .~ renderMustache missingHoursEmailTemplate params ^. strict
                                case x of
                                    Left exc -> logAttention "sendEmail failed" (show exc)
                                    Right () -> return ()

                        when (pref ^. prefHoursPingSMS) $ case p ^. P.employeeWorkPhone of
                            Nothing -> logAttention "Employee without phone" login
                            Just numb -> do
                                x <- liftIO $ tryDeep $ S.sendSms mgr smsProxyBurl $ S.Req numb $
                                    renderMustache missingHoursSmsTemplate params ^. strict
                                case x of
                                    Left exc -> logAttention "sendSms failed" (show exc)
                                    Right _  -> return ()

        return "OK"
  where
    mgr = ctxManager ctx
    lgr = ctxLogger ctx
    cfg = ctxConfig ctx

    preferencesBurl = cfgPreferencesAppBaseurl cfg
    emailProxyBurl  = cfgEmailProxyBaseurl cfg
    smsProxyBurl    = cfgSmsProxyBaseurl cfg
