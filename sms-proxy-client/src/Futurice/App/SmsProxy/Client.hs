module Futurice.App.SmsProxy.Client where

import Futurice.Prelude
import Prelude ()
import Servant.API
import Servant.Client
import Futurice.App.SmsProxy.API
import Futurice.App.SmsProxy.Types

sendSms :: Manager -> BaseUrl -> Req -> IO Res
sendSms mgr burl req =
    runClientM (impl req) (mkClientEnv mgr burl) >>= either throwM pure
  where
    _ :<|> _ :<|> impl = client smsProxyApi
