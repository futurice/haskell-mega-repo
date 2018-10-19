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
import Data.Pool                       (withResource)
import Data.Text.Encoding              (decodeLatin1)
import Futurice.Metrics.RateMeter      (mark)
import Futurice.Postgres               (createPostgresPool)
import Futurice.Prelude
import Futurice.Servant
import Network.Wai                     (Request, rawPathInfo)
import Network.Wai.Middleware.HttpAuth (basicAuth')
import Prelude ()
import Servant
import Text.Regex.Applicative.Text     (RE', anySym, match, string)

import qualified Data.Swagger               as Sw
import qualified Data.Text                  as T
import qualified Database.PostgreSQL.Simple as PQ
import qualified Futurice.KleeneSwagger     as K

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
              (K.operationsMatching $ K.sym "personio-request" <|> K.psym (isSuffixOf "-haxl"))
              [ "Raw data access" ]

checkCreds :: Ctx -> Request -> ByteString -> ByteString -> IO Bool
checkCreds ctx req u p = withResource (ctxPostgresPool ctx) $ \conn -> do
    let u' = decodeLatin1 u
        p' = decodeLatin1 p
        endpoint = decodeLatin1 $ rawPathInfo req
    case match isDocumentationRegexp endpoint of
        Nothing -> regularCheck conn u' p' endpoint
        Just _  -> swaggerCheck conn u' p' endpoint
  where
    regularCheck :: PQ.Connection -> Text -> Text -> Text -> IO Bool
    regularCheck conn u' p' endpoint = do
        res <- PQ.query conn (fromString credentialAndEndpointCheck)
            (u', p', endpoint) :: IO [PQ.Only Int]
        case res of
            []    -> logInvalidLogin u' endpoint >> pure False
            _ : _ -> logAccess conn u' endpoint >> pure True

    swaggerCheck :: PQ.Connection -> Text -> Text -> Text -> IO Bool
    swaggerCheck conn u' p' endpoint = do
        res <- PQ.query conn (fromString credentialCheck)
            (u', p') :: IO [PQ.Only Int]
        case res of
            []    -> logInvalidLogin u' endpoint >> pure False
            _ : _ -> pure True

    logInvalidLogin :: Text -> Text -> IO ()
    logInvalidLogin u' endpoint = do
        mark $ "Invalid login"
        runLogT "checkCreds" (ctxLogger ctx) $ do
            logAttention "Invalid login with " $ object
                [ "username" .= u'
                , "endpoint" .= endpoint
                ]

    -- | Logs user, and requested endpoint if endpoint is not swagger-related.
    logAccess :: PQ.Connection -> Text -> Text -> IO ()
    logAccess conn user endpoint =
        when (isNothing $ match isDocumentationRegexp endpoint) $ void $ do
            mark $ "endpoint " <> endpoint
            PQ.execute conn
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

    credentialCheck :: String
    credentialCheck = unwords
        [ "SELECT 1 FROM proxyapp.credentials"
        , "WHERE username = ? AND passtext = crypt(?, passtext)"
        , ";"
        ]

    credentialAndEndpointCheck :: String
    credentialAndEndpointCheck = unwords
        [ "SELECT 1 from proxyapp.credentials c, proxyapp.policy_endpoint pe"
        , "WHERE c.policyname = pe.policyname"
        , "AND c.username = ? AND passtext = crypt(?, passtext)"
        , "AND ? LIKE pe.endpoint || '%'"
        , ";"
        ]
