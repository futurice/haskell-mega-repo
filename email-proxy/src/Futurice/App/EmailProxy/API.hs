{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.EmailProxy.API where

import Futurice.Prelude
import Prelude ()
import Servant
import Futurice.App.EmailProxy.Types

type EmailProxyAPI =
    Summary "Index page, simple text" :> Get '[PlainText] Text

    :<|> Summary "Simple email sending"
        :> Description "Recipient lists shall be non-empty. Uses AWS SES as a backend."
        :> "send" :> ReqBody '[JSON] Req :> Post '[JSON] NoContent

emailProxyApi :: Proxy EmailProxyAPI
emailProxyApi = Proxy
