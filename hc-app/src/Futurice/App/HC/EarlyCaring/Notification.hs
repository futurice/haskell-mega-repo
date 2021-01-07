{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.HC.EarlyCaring.Notification where

import Data.List                      (group)
import Futurice.App.EmailProxy.Client (sendEmail)
import Futurice.App.EmailProxy.Types
       (emptyReq, fromEmail, reqBody, reqCc, reqSubject)
import Futurice.Integrations
       (MonadPersonio, MonadPlanMillQuery, beginningOfPrev2Month, personio,
       planmillEmployee, runIntegrations)
import Futurice.Prelude
import Numeric.Interval.NonEmpty      (Interval, (...))
import Prelude ()

import Futurice.App.HC.Config
import Futurice.App.HC.Ctx
import Futurice.App.HC.EarlyCaring.Page
import Futurice.App.HC.EarlyCaring.Template
import Futurice.App.HC.EarlyCaring.Types

import qualified Data.List.NonEmpty as NE
import qualified Personio           as P
import qualified PlanMill           as PM
import qualified PlanMill.Queries   as PMQ

pmData :: (MonadPlanMillQuery m, MonadPersonio m) => Interval Day -> m [EarlyCaringPlanMill]
pmData interval = do
    -- todo: make personio + planmill map function.
    us <- PMQ.users
    fmap catMaybes $ for (toList us) $ \u -> do
        let uid = u ^. PM.identifier
        for (PM.userLogin u) $ \login -> EarlyCaringPlanMill login uid
            <$> planmillEmployee uid
            <*> PMQ.capacities interval uid
            <*> PMQ.timereports interval uid
            <*> PMQ.userTimebalance uid

data SendStatus = Successful
                | SendFailed
                | NoEmail
                | SupervisorNotFound
                | NoEmployees
                deriving (Eq, Ord, Show)

sendEarlyCaringNotification :: Ctx -> IO ()
sendEarlyCaringNotification ctx = do
    now <- currentTime
    today <- currentDay
    let interval = beginningOfPrev2Month today ... today
    (_, balances) <- liftIO $ runIntegrations mgr lgr now (cfgIntegrationsCfg cfg) $
        calculateBalances today
        <$> personio P.PersonioEmployees
        <*> pmData interval
        <*> PMQ.absences
        <*> PMQ.allEnumerationValues Proxy Proxy
    stats <- for balances $ \bs -> do
        if emptyTemplate today interval (toList bs) then
          return NoEmployees
        else do
            let ms = balanceSupervisor $ NE.head bs
            runLogT "early-caring-submit" lgr $ do
                case ms of
                  Nothing -> return SupervisorNotFound
                  Just s ->
                      case  s ^. P.employeeEmail of
                        Nothing -> return NoEmail
                        Just email -> do
                            let toAddr = email
                            let body   = renderTemplate
                                    (s ^. P.employeeFirst)
                                    today
                                    interval
                                    (toList bs)
                            let req = emptyReq (fromEmail toAddr)
                                    & reqSubject .~ "Early caring email"
                                    & reqBody    .~ body ^. strict
                                    & reqCc      .~ fmap (pure . fromEmail) (cfgEarlyCaringCC cfg)
                            x <- liftIO $ tryDeep $ sendEmail mgr (cfgEmailProxyBaseurl cfg) req
                            case x of
                              Left exc -> logAttention "sendEmail failed" (show exc) >> return SendFailed
                              Right () -> return Successful
    pure ()
  where

    cfg = ctxConfig ctx
    mgr = ctxManager ctx
    lgr = ctxLogger ctx
