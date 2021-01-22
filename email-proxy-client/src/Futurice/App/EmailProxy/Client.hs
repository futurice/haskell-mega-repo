module Futurice.App.EmailProxy.Client (sendEmail, sendHtmlEmail) where

import Futurice.App.EmailProxy.API
import Futurice.App.EmailProxy.Types
import Futurice.Prelude
import Prelude ()
import Servant.API
import Servant.Client

sendEmail :: Manager -> BaseUrl -> Req -> IO ()
sendEmail mgr burl req =
    runClientM (impl req) (mkClientEnv mgr burl) >>= either throwM (\NoContent -> return ())
  where
    _ :<|> impl :<|> _ = client emailProxyApi

sendHtmlEmail :: Manager -> BaseUrl -> Req -> IO ()
sendHtmlEmail mgr burl req =
    runClientM (impl req) (mkClientEnv mgr burl) >>= either throwM (\NoContent -> return ())
  where
    _ :<|> _ :<|> impl = client emailProxyApi
