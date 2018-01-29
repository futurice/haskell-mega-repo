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

import Futurice.FUM.MachineAPI   (FUM6 (..), fum6)
import Futurice.Integrations     (personio, runIntegrations)
import Futurice.Lucid.Foundation (HtmlPage, fullRow_, h1_, page_)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant

import qualified Data.Set        as Set
import qualified FUM.Types.Login as FUM
import qualified Personio        as P

import Futurice.App.HC.API
import Futurice.App.HC.Config
import Futurice.App.HC.Ctx
import Futurice.App.HC.IndexPage
import Futurice.App.HC.PersonioValidation
import Futurice.App.HC.PrivateContacts

server :: Ctx -> Server HCAPI
server ctx = indexPageAction ctx
    :<|> personioValidationAction ctx
    :<|> personioPrivateContactsAction ctx

indexPageAction
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "index-page")
indexPageAction ctx mfu = case mfu <|> cfgMockUser cfg of
    -- TODO: access control
    Just fu -> return (indexPage fu)
    _       -> return page404
  where
    cfg = ctxConfig ctx

personioValidationAction
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "personio-validation")
personioValidationAction ctx mfu = case mfu <|> cfgMockUser cfg of
    Nothing -> return page404
    Just fu -> do
        now <- currentTime
        liftIO $ runIntegrations mgr lgr now (cfgIntegrationsCfg cfg) $ do
            fus <- mconcat <$> traverse (fum6 . FUMGroupEmployees) (cfgAccessGroups cfg)
            if fu `Set.notMember` fus
            then return page404
            else do
                today <- currentDay
                vs <- personio P.PersonioValidations
                return (validationReport vs today)
  where
    cfg = ctxConfig ctx
    mgr = ctxManager ctx
    lgr = ctxLogger ctx

personioPrivateContactsAction
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "private-contacts")
personioPrivateContactsAction ctx mfu = case mfu <|> cfgMockUser cfg of
    Nothing -> return page404
    Just fu -> do
        now <- currentTime
        liftIO $ runIntegrations mgr lgr now (cfgIntegrationsCfg cfg) $ do
            fus <- mconcat <$> traverse (fum6 . FUMGroupEmployees) (cfgAccessGroups cfg)
            if fu `Set.notMember` fus
            then return page404
            else do
                es <- personio P.PersonioEmployees
                return (privateContacts es)
  where
    cfg = ctxConfig ctx
    mgr = ctxManager ctx
    lgr = ctxLogger ctx

page404 :: HtmlPage a
page404 = page_ "HC - Unauthorised" $
    fullRow_ $ do
        h1_ "Unauthorised"
        "Ask IT Team for access rights"

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName        .~ "HC"
    & serverDescription .~ "Tools for HC"
    & serverApp hcApi   .~ server
    & serverColour      .~  (Proxy :: Proxy ('FutuAccent 'AF3 'AC2))
    & serverEnvPfx      .~ "HCAPP"
  where
    makeCtx :: () -> Config -> Logger -> Manager -> Cache -> IO (Ctx, [Job])
    makeCtx () cfg lgr mgr _cache = do
        let ctx = Ctx cfg lgr mgr
        pure (ctx, [])
