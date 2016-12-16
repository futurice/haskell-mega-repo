{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.FutuhoursMock (defaultMain) where

import Futurice.Prelude
import Prelude ()

import Futurice.Servant
import Servant

import Futurice.App.FutuhoursMock.API
import Futurice.App.FutuhoursMock.Config (Config (..))
import Futurice.App.FutuhoursMock.Logic  (projectEndpoint, userEndpoint,
                                         hoursEndpoint, entryEndpoint,
                                         entryIdEndpoint, entryDeleteEndpoint)
import Futurice.App.FutuhoursMock.Types

server :: Ctx -> Server FutuhoursAPI 
server ctx = pure "This is futuhours mock api"
    :<|> liftIO (projectEndpoint ctx)
    :<|> liftIO (userEndpoint ctx)
    :<|> hoursEndpoint ctx
    :<|> entryEndpoint ctx
    :<|> entryIdEndpoint ctx
    :<|> entryDeleteEndpoint ctx

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName            .~ "Futuhours MOCK api"
    & serverDescription     .~ "Is it real?"
    & serverApp futuhoursApi .~ server
    & serverColour          .~  (Proxy :: Proxy ('FutuAccent 'AF2 'AC2))
    & serverEnvPfx          .~ "FUTUHOURSMOCK"
  where
    makeCtx :: Config -> Logger -> DynMapCache -> IO Ctx
    makeCtx _ _ _ = do
        pure Ctx
