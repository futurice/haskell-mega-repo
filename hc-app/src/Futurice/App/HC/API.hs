{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.HC.API where

import Futurice.Prelude
import Futurice.Servant
import Futurice.Lucid.Foundation (HtmlPage)
import Prelude ()
import Servant

type HCAPI = SSOUser :> Get '[HTML] (HtmlPage "index-page")
    :<|> SSOUser :> "personio-validation" :> Get '[HTML] (HtmlPage "personio-validation")
    :<|> SSOUser :> "private-contacts" :> Get '[HTML] (HtmlPage "private-contacts")

hcApi :: Proxy HCAPI
hcApi = Proxy
