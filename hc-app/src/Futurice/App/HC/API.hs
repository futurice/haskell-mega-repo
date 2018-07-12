{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.HC.API where

import Futurice.App.HC.EarlyCaring.Types (EarlyCaringEmail, SignedBlob)
import Futurice.Lucid.Foundation         (HtmlPage)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant
import Servant.API.Generic

data Record route = Record
    { recIndex               :: route :- SSOUser
        :> Get '[HTML] (HtmlPage "index-page")
    , recPersonioValidations :: route :- SSOUser
        :> "personio-validation"
        :> Get '[HTML] (HtmlPage "personio-validation")
    , recPrivateContacts     :: route :- SSOUser
        :> "private-contacts"
        :> Get '[HTML] (HtmlPage "private-contacts")
    , recAnniversaries       :: route :- SSOUser
        :> "anniversaries"
        :> Get '[HTML] (HtmlPage "anniversaries")
    , recHrNumbers           :: route :- SSOUser
        :> "hr-numbers"
        :> Get '[HTML] (HtmlPage "hr-numbers")
    , recEarlyCaring         :: route :- SSOUser
        :> "early-caring"
        :> Get '[HTML] (HtmlPage "early-caring")
    , recEarlyCaringSubmit   :: route :- SSOUser
        :> "early-caring-submit"
        :> ReqBody '[JSON] (SignedBlob EarlyCaringEmail)
        :> Post '[JSON] Bool
    }
  deriving Generic

type HCAPI = ToServantApi Record

hcApi :: Proxy HCAPI
hcApi = genericApi (Proxy :: Proxy Record)
