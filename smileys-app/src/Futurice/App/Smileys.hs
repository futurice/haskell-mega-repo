{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Smileys (defaultMain) where

import Futurice.Prelude
import Prelude ()

import Futurice.Servant
import Futurice.Postgres
import Servant

-- Smileys modules
import Futurice.App.Smileys.API
import Futurice.App.Smileys.Charts
import Futurice.App.Smileys.Config (Config (..))
import Futurice.App.Smileys.Ctx
import Futurice.App.Smileys.Logic

server :: Ctx -> Server SmileysAPI
server ctx = pure "smileys backend"
    :<|> postOwnSmileys ctx
    :<|> getOwnSmileys ctx
    :<|> getSmileys ctx
    :<|> absoluteChartHandler ctx
    :<|> relativeChartHandler ctx

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService        .~ SmileysApiService
    & serverDescription    .~ "Hours Smileys"
    & serverColour         .~ (Proxy :: Proxy ('FutuAccent 'AF5 'AC2))
    & serverApp smileysApi .~ server
    & serverEnvPfx         .~ "SMILEYS"
  where
    makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
    makeCtx Config {..} logger _ cache _ = do
        pp <- createPostgresPool cfgPostgresConnInfo
        let ctx = Ctx
                  { ctxPostgresPool = pp
                  , ctxMockUser     = cfgMockUser
                  , ctxLogger       = logger
                  , ctxCache        = cache
                  }
        pure (ctx, [])
