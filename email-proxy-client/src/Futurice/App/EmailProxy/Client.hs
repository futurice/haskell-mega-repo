module Futurice.App.EmailProxy.Client (sendEmail) where

import Futurice.Prelude
import Prelude ()
import Servant.API
import Servant.Client
import Futurice.App.EmailProxy.API
import Futurice.App.EmailProxy.Types

sendEmail :: Manager -> BaseUrl -> Req -> IO ()
sendEmail mgr burl req =
    runClientM (impl req) (mkClientEnv mgr burl) >>= either throwM (\NoContent -> return ())
  where
    _ :<|> impl = client emailProxyApi
