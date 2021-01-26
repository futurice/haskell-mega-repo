{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.EmailProxy.API where

import Futurice.App.EmailProxy.Types
import Futurice.Prelude
import Prelude ()
import Servant.API

type EmailProxyAPI =
    Summary "Index page, simple text" :> Get '[PlainText] Text

    :<|> Summary "Simple email sending"
        :> Description "Recipient lists shall be non-empty. Uses AWS SES as a backend."
        :> "send" :> ReqBody '[JSON] Req :> Post '[JSON] NoContent
    :<|> Summary "Simple email sending with body being html"
        :> Description "Recipient lists shall be non-empty. Uses AWS SES as a backend."
        :> "send-html" :> ReqBody '[JSON] Req :> Post '[JSON] NoContent

emailProxyApi :: Proxy EmailProxyAPI
emailProxyApi = Proxy
