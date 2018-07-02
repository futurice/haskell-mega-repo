{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
module Futurice.App.HC.Main (defaultMain) where

import Futurice.App.EmailProxy.Client (sendEmail)
import Futurice.App.EmailProxy.Types
       (emptyReq, fromEmail, reqBody, reqCc, reqSubject)
import Futurice.FUM.MachineAPI        (FUM6 (..), fum6)
import Futurice.Integrations
       (Integrations, beginningOfPrev2Month, personio, planmillEmployee,
       runIntegrations)
import Futurice.Lucid.Foundation      (HtmlPage, fullRow_, h1_, page_)
import Futurice.Prelude
import Futurice.Servant
import Numeric.Interval.NonEmpty      ((...))
import Prelude ()
import Servant
import System.Entropy                 (getEntropy)

import qualified Data.Set         as Set
import qualified FUM.Types.Login  as FUM
import qualified Personio         as P
import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ

import Futurice.App.HC.API
import Futurice.App.HC.Anniversaries
import Futurice.App.HC.Config
import Futurice.App.HC.Ctx
import Futurice.App.HC.EarlyCaring.Page
import Futurice.App.HC.EarlyCaring.Types
import Futurice.App.HC.IndexPage
import Futurice.App.HC.HRNumbers
import Futurice.App.HC.PersonioValidation
import Futurice.App.HC.PrivateContacts

server :: Ctx -> Server HCAPI
server ctx = indexPageAction ctx
    :<|> personioValidationAction ctx
    :<|> personioPrivateContactsAction ctx
    :<|> anniversariesAction ctx
    :<|> hrnumbersAction ctx
    :<|> earlyCaringAction ctx
    :<|> earlyCaringSubmitAction ctx

withAuthUser
    :: (MonadIO m, MonadTime m)
    => (FUM.Login -> Integrations '[I, Proxy, I, Proxy, Proxy, I] (HtmlPage a))
    -> Ctx -> Maybe FUM.Login
    -> m (HtmlPage a)
withAuthUser = withAuthUser' page404

withAuthUser'
    :: (MonadIO m, MonadTime m)
    => a
    -> (FUM.Login -> Integrations '[I, Proxy, I, Proxy, Proxy, I] a)
    -> Ctx -> Maybe FUM.Login
    -> m a
withAuthUser' def action ctx mfu = case mfu <|> cfgMockUser cfg of
    Nothing -> return def
    Just fu -> do
        now <- currentTime
        liftIO $ runIntegrations mgr lgr now (cfgIntegrationsCfg cfg) $ do
            fus <- mconcat <$> traverse (fum6 . FUMGroupEmployees) (cfgAccessGroups cfg)
            if fu `Set.notMember` fus
            then return def
            else action fu
  where
    cfg = ctxConfig ctx
    mgr = ctxManager ctx
    lgr = ctxLogger ctx

indexPageAction
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "index-page")
indexPageAction = withAuthUser (return . indexPage)

personioValidationAction
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "personio-validation")
personioValidationAction = withAuthUser $ \_ -> do
    today <- currentDay
    vs <- personio P.PersonioValidations
    return (validationReport vs today)

personioPrivateContactsAction
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "private-contacts")
personioPrivateContactsAction = withAuthUser $ \_ -> do
    es <- personio P.PersonioEmployees
    return (privateContacts es)

anniversariesAction
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "anniversaries")
anniversariesAction = withAuthUser $ \_ -> do
    today <- currentDay
    es <- personio P.PersonioEmployees
    return (anniversaries es today)

hrnumbersAction
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "hr-numbers")
hrnumbersAction = withAuthUser $ \_ -> do
    today <- currentDay
    es <- personio P.PersonioEmployees
    return (hrnumbers es today)

earlyCaringAction
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "early-caring")
earlyCaringAction ctx = impl ctx
  where
    impl = withAuthUser $ \_ -> do
        today <- currentDay
        let interval = beginningOfPrev2Month today ... today
        earlyCaringPage secret today interval
            <$> personio P.PersonioEmployees
            <*> pmData interval
            <*> PMQ.absences
            <*> PMQ.allEnumerationValues Proxy Proxy

    secret = ctxSecret ctx

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

earlyCaringSubmitAction
    :: Ctx
    -> Maybe FUM.Login
    -> SignedBlob EarlyCaringEmail
    -> Handler Bool
earlyCaringSubmitAction ctx mfu sb = do
    x <- withAuthUser' False (const $ return True) ctx mfu
    if x then liftIO impl else return False
  where
    cfg = ctxConfig ctx
    mgr = ctxManager ctx
    lgr = ctxLogger ctx

    impl = runLogT "early-caring-submit" lgr $ 
        case verifySignedBlob (ctxSecret ctx) sb of
            Left err     -> do
                logAttention "Cannot verify data" err
                return False
            Right (EarlyCaringEmail toAddr subject body) -> do
                x <- liftIO $ tryDeep $ sendEmail mgr (cfgEmailProxyBaseurl cfg) req
                case x of
                    Left exc -> logAttention "sendEmail failed" (show exc) >> return False
                    Right () -> return True
              where
                req = emptyReq (fromEmail toAddr)
                    & reqSubject .~ subject
                    & reqBody    .~ body ^. strict
                    & reqCc      .~ fmap (pure . fromEmail) (cfgEarlyCaringCC cfg)


page404 :: HtmlPage a
page404 = page_ "HC - Unauthorised" $
    fullRow_ $ do
        h1_ "Unauthorised"
        "Ask IT Team for access rights"

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverService     .~ HCService
    & serverDescription .~ "Tools for HC"
    & serverApp hcApi   .~ server
    & serverColour      .~ (Proxy :: Proxy ('FutuAccent 'AF3 'AC2))
    & serverEnvPfx      .~ "HCAPP"
  where
    makeCtx :: () -> Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
    makeCtx () cfg lgr mgr _cache _mq = do
        secret <- getEntropy 64
        let ctx = Ctx cfg lgr mgr secret
        pure (ctx, [])
