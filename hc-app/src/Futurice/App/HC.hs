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
module Futurice.App.HC (defaultMain) where

import Prelude ()
import Futurice.Prelude
import Futurice.Servant
import Futurice.Lucid.Foundation (HtmlPage, h1_, fullRow_, page_)
import Servant

import qualified FUM.Types.Login as FUM

import Futurice.App.HC.API
import Futurice.App.HC.Config
import Futurice.App.HC.Ctx
import Futurice.App.HC.IndexPage

server :: Ctx -> Server HCAPI
server ctx = indexPageAction ctx

indexPageAction
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "index-page")
indexPageAction ctx mfu = case mfu <|> cfgMockUser cfg of
    -- TODO: access control
    Just fu -> return (indexPage fu)
    _       -> return page404
  where
    cfg = ctx

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
    makeCtx () cfg _lgr _mgr _cache = do
        let ctx = cfg
        pure (ctx, [])
