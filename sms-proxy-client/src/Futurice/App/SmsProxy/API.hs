{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.SmsProxy.API where

import Futurice.Prelude
import Prelude ()
import Servant.API
import Futurice.App.SmsProxy.Types

type SmsProxyAPI =
    Get '[PlainText] Text
    :<|> "send" :> QueryParam "to" Text :> QueryParam "text" Text :> Get '[PlainText] Text
    :<|> "send" :> ReqBody '[JSON] Req :> Post '[JSON] Res

smsProxyApi :: Proxy SmsProxyAPI
smsProxyApi = Proxy
