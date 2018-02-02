{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.HC.API where

import Futurice.Prelude
import Futurice.Servant
import Futurice.Lucid.Foundation (HtmlPage)
import Prelude ()
import Servant
import Futurice.App.HC.EarlyCaring.Types (SignedBlob, EarlyCaringEmail)

type HCAPI = SSOUser :> Get '[HTML] (HtmlPage "index-page")
    :<|> SSOUser :> "personio-validation" :> Get '[HTML] (HtmlPage "personio-validation")
    :<|> SSOUser :> "private-contacts" :> Get '[HTML] (HtmlPage "private-contacts")
    :<|> SSOUser :> "early-caring" :> Get '[HTML] (HtmlPage "early-caring")
    :<|> SSOUser :> "early-caring-submit" :> ReqBody '[JSON] (SignedBlob EarlyCaringEmail) :> Post '[JSON] Bool

hcApi :: Proxy HCAPI
hcApi = Proxy
