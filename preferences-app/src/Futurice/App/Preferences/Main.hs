{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Futurice.App.Preferences.Main (defaultMain) where

import Data.Aeson                 (Value (..))
import Data.List                  (foldl')
import Database.PostgreSQL.Simple (In (..))
import FUM.Types.Login            (Login, mkLogin)
import Futurice.Lucid.Foundation  (HtmlPage)
import Futurice.Postgres
       (Only (..), createPostgresPool, safePoolExecute, safePoolQuery)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant


import qualified Data.Map as Map

import Futurice.App.Preferences.Config
import Futurice.App.Preferences.Ctx
import Futurice.App.Preferences.IndexPage
import Futurice.App.Preferences.Types

indexPageImpl :: Ctx -> Maybe Login -> Handler (HtmlPage "index")
indexPageImpl _ctx  Nothing     = indexPageImpl _ctx (Just $(mkLogin "xxxx"))
indexPageImpl  ctx (Just login) = runLogT "index-page" (ctxLogger ctx) $ do
    res <- safePoolQuery ctx
        "SELECT preference, value FROM preferences.values WHERE login = ? "
        (Only login)
    return $ indexPage (makePreferences defaultPreferences res)

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

makePreferences :: Preferences -> [(Text, Value)] -> Preferences
makePreferences = foldl' go where
    go :: Preferences -> (Text, Value) -> Preferences
    go pref ("hours-ping-sms",   Bool b) = pref & prefHoursPingSMS .~ b
    go pref ("hours-ping-email", Bool b) = pref & prefHoursPingEmail .~ b
    go pref _ = pref

-------------------------------------------------------------------------------
-- Setting update
-------------------------------------------------------------------------------

boolPreference
    :: Ctx
    -> Text
    -> Maybe Login
    -> Bool
    -> Handler (CommandResponse ())
boolPreference ctx k Nothing      v = boolPreference ctx k (Just $(mkLogin "xxxx")) v
boolPreference ctx k (Just login) v = runLogT "bool-preference" (ctxLogger ctx) $ do
    _res <- safePoolExecute ctx query (login, k, Bool v)
    return CommandResponseReload
  where
    query = fromString $ unwords
        [ "INSERT INTO preferences.values (login, preference, value)"
        , "VALUES (?, ?, ?) ON CONFLICT (login, preference) DO UPDATE"
        , "SET value = EXCLUDED.value, updated = now()"
        , ";"
        ]

-------------------------------------------------------------------------------
-- Getter
-------------------------------------------------------------------------------

getPreferencesImpl :: Ctx -> [Login] -> Handler (Map Login Preferences)
getPreferencesImpl ctx logins = runLogT "get-preferences" (ctxLogger ctx) $ do
    res <- safePoolQuery ctx
        "SELECT login, preference, value FROM preferences.values WHERE login in ?;"
        (Only (In logins))
    return
        $ fmap (makePreferences defaultPreferences)
        $ Map.fromListWith (++)
        $ map (\(a,b,c) -> (a,[(b,c)])) res

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

server :: Ctx -> Server API
server ctx = indexPageImpl ctx
    :<|> boolPreference ctx "hours-ping-sms"
    :<|> boolPreference ctx "hours-ping-email"
    :<|> getPreferencesImpl ctx

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService      .~ PreferencesService
    & serverDescription  .~ "Preferences"
    & serverColour       .~ (Proxy :: Proxy ('FutuAccent 'AF6 'AC1))
    & serverApp api      .~ server
    & serverEnvPfx       .~ "PREFERENCES"
  where
    makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
    makeCtx cfg lgr _mgr _cache _mq = do
        pgPool <- createPostgresPool $ cfgPostgresConnInfo cfg
        return (Ctx lgr pgPool, [])
