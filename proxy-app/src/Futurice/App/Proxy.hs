{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Futurice.App.Proxy (
    defaultMain,
    ) where

import Data.Aeson.Compat               (object, (.=))
import Data.List                       (isSuffixOf)
import Data.Maybe                      (isNothing)
import Data.Text.Encoding              (decodeLatin1)
import Futurice.Metrics.RateMeter      (mark)
import Futurice.Postgres               (createPostgresPool, safePoolExecute)
import Futurice.Postgres.SqlBuilder
import Futurice.Prelude
import Futurice.Servant
import Network.Wai                     (Request, rawPathInfo)
import Network.Wai.Middleware.HttpAuth (basicAuth')
import Prelude ()
import Servant
import Text.Regex.Applicative.Text     (RE', anySym, match, string)

import qualified Data.Swagger           as Sw
import qualified Data.Text              as T
import qualified Futurice.KleeneSwagger as K

import Futurice.App.Proxy.API
import Futurice.App.Proxy.Config
import Futurice.App.Proxy.Ctx
import Futurice.App.Proxy.Endpoint (proxyServer)
import Futurice.App.Proxy.Markup

-------------------------------------------------------------------------------
-- WAI/startup
------------------------------------------------------------------------------

server :: Ctx -> Server ProxyAPI
server ctx = -- pure "P-R-O-X-Y" :<|>
    proxyServer ctx (Proxy :: Proxy Routes)

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService         .~ ProxService
    & serverDescription     .~ "Proxy from the outer space"
    & serverColour          .~ (Proxy :: Proxy ('FutuAccent 'AF6 'AC3))
    & serverHtmlApp htmlApi .~ htmlServer
    & serverApp proxyAPI    .~ server
    & serverMiddleware      .~ (\ctx -> basicAuth' (checkCreds ctx) "P-R-O-X-Y")
    & serverEnvPfx          .~ "PROX"
    & serverSwaggerMod      .~ swaggerMod
  where
    makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
    makeCtx cfg@Config {..} lgr _mgr _cache _mq = do
        mgr                  <- newManager tlsManagerSettings
        postgresPool         <- createPostgresPool cfgPostgresConnInfo
        pure $ flip (,) [] Ctx
            { _ctxManager     = mgr
            , ctxPostgresPool = postgresPool
            , ctxLogger       = lgr
            , _ctxConfig      = cfg
            }

    swaggerMod
        = Sw.applyTagsFor
            (K.operationsMatching $ K.sym "power" *> many K.anySym)
            [ "Power reports" ]

        . Sw.applyTagsFor
            (K.operationsMatching $ K.sym "futuqu" *> many K.anySym)
            [ "futuqu" ]

        . Sw.applyTagsFor
              (K.operationsMatching $ K.sym "personio-request" <|> K.psym (isSuffixOf "-haxl"))
              [ "Raw data access" ]

        . Sw.applyTagsFor
            (K.operationsMatching $ K.sym "library" *> many K.anySym)
            [ "Library" ]

checkCreds :: Ctx -> Request -> ByteString -> ByteString -> IO Bool
checkCreds ctx req u p = do
    let u' = decodeLatin1 u
        p' = decodeLatin1 p
        endpoint = decodeLatin1 $ rawPathInfo req

    runLogT "check-creds" (ctxLogger ctx) $ case match isDocumentationRegexp endpoint of
        Nothing -> regularCheck u' p' endpoint
        Just _  -> swaggerCheck u' p' endpoint
  where
    regularCheck :: Text -> Text -> Text -> LogT IO Bool
    regularCheck u' p' endpoint = do
        res <- safePoolQueryM ctx "proxyapp" $ do
            creds <- from_ "credentials"
            where_ [ ecolumn_ creds "username", " = ", eparam_ u' ]
            where_ [ ecolumn_ creds "passtext", " = crypt(", eparam_ p', ", ", ecolumn_ creds "passtext", ")" ]
            endpoints <- from_ "policy_endpoint"
            where_ [ ecolumn_ creds "policyname", " = ", ecolumn_ endpoints "policyname" ]
            where_ [ eparam_ endpoint, " LIKE ", ecolumn_ endpoints "endpoint", " || '%'" ]

        case res :: [Only Int] of
            []    -> logInvalidLogin u' endpoint >> pure False
            _ : _ -> logAccess u' endpoint >> pure True

    swaggerCheck :: Text -> Text -> Text -> LogT IO Bool
    swaggerCheck u' p' endpoint = do
        res <- safePoolQueryM ctx "proxyapp" $ do
            creds <- from_ "credentials"
            where_ [ ecolumn_ creds "username", " = ", eparam_ u' ]
            where_ [ ecolumn_ creds "passtext", " = crypt(", eparam_ p', ", ", ecolumn_ creds "passtext", ")" ]

        case res :: [Only Int] of
            []    -> logInvalidLogin u' endpoint >> pure False
            _ : _ -> pure True

    logInvalidLogin :: Text -> Text -> LogT IO ()
    logInvalidLogin u' endpoint = do
        liftIO $ mark $ "Invalid login"
        logAttention "Invalid login with " $ object
            [ "username" .= u'
            , "endpoint" .= endpoint
            ]

    -- | Logs user, and requested endpoint if endpoint is not swagger-related.
    logAccess :: Text -> Text -> LogT IO ()
    logAccess user endpoint =
        when (isNothing $ match isDocumentationRegexp endpoint) $ void $ do
            liftIO $ mark $ "endpoint " <> endpoint
            safePoolExecute ctx
                "insert into proxyapp.accesslog (username, endpoint) values (?, ?);"
                (user, endpoint)

    isDocumentationRegexp :: RE' Text
    isDocumentationRegexp = choice
        [ string "/"
        , string "/favicon.ico"
        , string "/swagger.json"
        , string "/swagger-ui" *> (T.pack <$> many anySym)
        , string "/vendor/" *> (T.pack <$> many anySym)
        ]

    choice = foldr (<|>) empty
