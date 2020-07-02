{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.HC.API where

import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant
import Servant.API.Generic
import Servant.Chart             (Chart, SVG)

import Futurice.App.HC.Achoo.Types       (AchooChart)
import Futurice.App.HC.EarlyCaring.Types
       (BalanceCSV, EarlyCaringEmail, SignedBlob)

import qualified Personio as P

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
        :> QueryFlag "supervisor"
        :> Get '[HTML] (HtmlPage "early-caring")
    , recEarlyCaringCSV      :: route :- SSOUser
        :> "early-caring.csv"
        :> QueryFlag "supervisor"
        :> Get '[CSV] [BalanceCSV]
    , recEarlyCaringSubmit   :: route :- SSOUser
        :> "early-caring-submit"
        :> ReqBody '[JSON] (SignedBlob EarlyCaringEmail)
        :> Post '[JSON] Bool
    -- achoo report
    , recAchooReport        :: route :- SSOUser
        :> "achoo-report"
        :> QueryParam "from" Day
        :> QueryParam "to"   Day
        :> QueryParam "whole" Bool
        :> Get '[HTML] (HtmlPage "achoo-report")
    , recAchooChart         ::  route :- SSOUser
        :> "achoo-chart"
        :> Capture "type" AchooChart
        :> QueryParam' '[ Required ] "from" Day
        :> QueryParam' '[ Required ] "type" Day
        :> QueryParam' '[ Required ] "whole" Bool
        :> Get '[SVG] (Chart "achoo-chart")
    , recVacationReport :: route :- SSOUser
        :> "vacation-report"
        :> Get '[HTML] (HtmlPage "vacation-report")
    , recVacationReportEmail :: route :- SSOUser
        :> "vacation-report"
        :> Capture "employeeId" P.EmployeeId
        :> Get '[HTML] (HtmlPage "vacation-report-single")
    , recVacationReportSubmit :: route :- SSOUser
        :> "vacation-report-submit"
        :> Post '[JSON] (CommandResponse ())
    }
  deriving Generic

type HCAPI = ToServantApi Record

hcApi :: Proxy HCAPI
hcApi = genericApi (Proxy :: Proxy Record)
