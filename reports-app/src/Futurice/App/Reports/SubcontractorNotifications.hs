{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Reports.SubcontractorNotifications where

import Data.Aeson                  (object, (.=))
import Data.Ord                    (comparing)
import Data.Time.Calendar          (addGregorianMonthsClip)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Futurice.Integrations       (beginningOfCurrMonth)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Reports.ActiveSubcontractors
import Futurice.App.Reports.Config
import Futurice.App.Reports.Ctx
import Futurice.App.Reports.Templates

import qualified Data.Vector                    as V
import qualified Futurice.App.EmailProxy.Client as E
import qualified Futurice.App.EmailProxy.Types  as E
import qualified Futurice.App.SmsProxy.Client   as S
import qualified Futurice.App.SmsProxy.Types    as S
import qualified Personio                       as P

checkNotificationDay :: Day -> LogT IO Text -> LogT IO Text
checkNotificationDay day m = do
    let days = [beginningOfCurrMonth day .. endOfCurrMonth day]
    let firstWeekDayOfMonth = head $ filter (\x -> (toWeekDate x) ^. _3 `notElem` [6,7]) days
    if day == firstWeekDayOfMonth then
      m
    else do
      logInfo "Not the first weekday of the month" day
      return "ERR: Other"
  where
    endOfCurrMonth = pred . addGregorianMonthsClip 1 . beginningOfCurrMonth

subcontractorNotifications :: Ctx -> IO Text
subcontractorNotifications ctx = runLogT "subcontractor-notifications" lgr $ do
    day <- currentDay
    checkNotificationDay day $ do
        (ActiveSubcontractorData _ activeSubcontractorData) <- liftIO $ runIntegrations' ctx activeSubcontractorsReport
        let subcontractors = nub $ sortBy (comparing $ view P.employeeFullname) $ V.toList $ V.map _asName activeSubcontractorData
        for_ subcontractors $ \p -> do
            let params = object
                    [ "name"     .= (p ^. P.employeeFirst)
                    ]
            case p ^. P.employeeEmail of
              Nothing -> logAttention "Subcontractor without email" (p ^. P.employeeFullname)
              Just addr -> do
                  x <- liftIO $ tryDeep $ E.sendEmail mgr emailProxyBurl $ E.emptyReq (E.fromEmail addr)
                      & E.reqSubject .~ "Billing reminder"
                      & E.reqBody    .~ renderMustache subcontractorEmailTemplate params ^. strict
                  case x of
                    Left exc -> logAttention "sendEmail failed" (show exc)
                    Right () -> return ()

            case p ^. P.employeeWorkPhone of
              Nothing -> logAttention "Subcontractor without phone" (p ^. P.employeeFullname)
              Just numb -> do
                  x <- liftIO $ tryDeep $ S.sendSms mgr smsProxyBurl $ S.Req numb $
                      renderMustache subcontractorSmsTemplate params ^. strict
                  case x of
                    Left exc -> logAttention "sendSms failed" (show exc)
                    Right _  -> return ()
        return "OK"
 where
    mgr = ctxManager ctx
    lgr = ctxLogger ctx
    cfg = ctxConfig ctx

    emailProxyBurl  = cfgEmailProxyBaseurl cfg
    smsProxyBurl    = cfgSmsProxyBaseurl cfg
