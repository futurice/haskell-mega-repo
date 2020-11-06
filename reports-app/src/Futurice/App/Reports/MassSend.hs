{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Reports.MassSend where

import Futurice.App.Reports.Config
import Futurice.App.Reports.Ctx
import Futurice.Prelude
import Prelude ()

import qualified Futurice.App.SmsProxy.Client as S
import qualified Futurice.App.SmsProxy.Types  as S
import qualified Personio                     as P

sendMessageToAll :: Ctx -> Text -> IO ()
sendMessageToAll ctx message = do
    employees <- runIntegrations' ctx $ P.personio P.PersonioEmployees
    let activeEmployees = filter (\p -> p ^. P.employeeStatus == P.Active && p ^. P.employeeEmploymentType == Just P.Internal) employees
    for_ activeEmployees $ \p ->
      case p ^. P.employeeWorkPhone of
        Nothing -> runLogT "missing-hours-notifications" lgr $ logAttention "Employee without phone" $ p ^. P.employeeLogin
        Just numb -> do
            x <- liftIO $ tryDeep $ S.sendSms mgr smsProxyBurl $ S.Req numb message
            case x of
              Left exc -> runLogT "missing-hours-notifications" lgr $ logAttention "sendSms failed" (show exc)
              Right _  -> return ()
    pure ()
  where
    cfg = ctxConfig ctx
    mgr = ctxManager ctx
    lgr = ctxLogger ctx
    smsProxyBurl    = cfgSmsProxyBaseurl cfg
