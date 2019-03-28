{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Reports.SubcontractorHoursNotifications where

import Data.Aeson                  (object, (.=))
import Data.Time.Calendar          (addGregorianMonthsClip)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Futurice.Integrations       (beginningOfCurrMonth)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Reports.Config
import Futurice.App.Reports.Ctx
import Futurice.App.Reports.Templates

import qualified Futurice.App.EmailProxy.Client as E
import qualified Futurice.App.EmailProxy.Types  as E
import qualified Personio                       as P

-- | Check that is the last non-weekend day of the month
checkNotificationsDay :: Day -> LogT IO Text -> LogT IO Text
checkNotificationsDay day m = do
    let days = [beginningOfCurrMonth day .. endOfCurrMonth day]
    let lastWeekDayOfMonth = last $ filter (\x -> toWeekDate x ^. _3 `notElem` [6,7]) days
    if day == lastWeekDayOfMonth then
      m
    else do
      logInfo "Not the last weekday of the month" day
      return "ERR: Other"
  where
    endOfCurrMonth = pred . addGregorianMonthsClip 1 . beginningOfCurrMonth

activeSubcontractorPredicate :: Day -> P.Employee -> Bool
activeSubcontractorPredicate _d p = and
    [ p ^. P.employeeEmploymentType == Just P.External
    , p ^. P.employeeStatus == P.Active
    ]

subcontractorHoursNotifications :: Ctx -> IO Text
subcontractorHoursNotifications ctx = runLogT "subcontractor-hours-notifications" lgr $ do
    day <- currentDay
    checkNotificationsDay day $ do
        subcontractors <- liftIO $ runIntegrations' ctx $ P.personio P.PersonioEmployees
        let subcontractors' = filter (activeSubcontractorPredicate day) subcontractors
        for_ subcontractors' $ \p -> do
            let params = object
                    [ "name"     .= (p ^. P.employeeFirst)
                    ]
            case p ^. P.employeeEmail of
              Nothing -> logAttention "Subcontractor without email" (p ^. P.employeeFullname)
              Just addr -> do
                  x <- liftIO $ tryDeep $ E.sendEmail mgr emailProxyBurl $ E.emptyReq (E.fromEmail addr)
                      & E.reqSubject .~ "Reminder: All hours for the month to be reported today"
                      & E.reqBody    .~ renderMustache subcontractorHoursEmailTemplate params ^. strict
                  case x of
                    Left exc -> logAttention "sendEmail failed" (show exc)
                    Right () -> return ()
        return "OK"
 where
    mgr = ctxManager ctx
    lgr = ctxLogger ctx
    cfg = ctxConfig ctx

    emailProxyBurl  = cfgEmailProxyBaseurl cfg
