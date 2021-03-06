{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.ProxyMgmt.API where

import Futurice.Lomake
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant
import Servant.API.Generic
import Servant.Chart             (Chart, SVG)

import Futurice.App.ProxyMgmt.Commands.AddEndpoint
import Futurice.App.ProxyMgmt.Commands.AddPolicy
import Futurice.App.ProxyMgmt.Commands.AddToken
import Futurice.App.ProxyMgmt.Commands.RemoveEndpoint
import Futurice.App.ProxyMgmt.Commands.UpdatePolicy
import Futurice.App.ProxyMgmt.Types

data ProxyMgmtRoutes route = ProxyMgmtRoutes
    { routeIndexPage :: route :-
        SSOUser :> Get '[HTML] (HtmlPage "index")
    , routeRegenerateOwnToken :: route :-
        SSOUser :> "regenerate-own-token" :> Post '[JSON] Text
    , routeRegenerateServiceToken :: route :-
        SSOUser :> "regenerate-service-token" :> ReqBody '[JSON] UserName :> Post '[JSON] Text
    -- Admin
    , routeTokensPage :: route :-
        SSOUser :> "tokens" :> Get '[HTML] (HtmlPage "tokens")
    , routePoliciesPage :: route :-
        SSOUser :> "policies" :> Get '[HTML] (HtmlPage "policies")
    , routeAuditPage :: route :-
        SSOUser :> "audit" :> Get '[HTML] (HtmlPage "audit")
    , routeReportsPage :: route :-
        SSOUser :> "reports" :> Get '[HTML] (HtmlPage "reports")
    -- Commands
    , routeAddPolicy :: route :-
        SSOUser :> "command" :> "add-policy"
        :> ReqBody '[JSON] (LomakeRequest AddPolicy)
        :> Post '[JSON] (CommandResponse ())
    , routeRemoveEndpoint :: route :-
        SSOUser :> "command" :> "remove-endpoint"
        :> ReqBody '[JSON] (LomakeRequest RemoveEndpoint)
        :> Post '[JSON] (CommandResponse ())
    , routeAddEndpoint :: route :-
        SSOUser :> "command" :> "add-endpoint"
        :> ReqBody '[JSON] (LomakeRequest AddEndpoint)
        :> Post '[JSON] (CommandResponse ())
    , routeAddToken :: route :-
        SSOUser :> "command" :> "add-token"
        :> ReqBody '[JSON] (LomakeRequest AddToken)
        :> Post '[JSON] (CommandResponse ())
    , routeUpdatePolicy :: route :-
        SSOUser :> "command" :> "update-policy"
        :> ReqBody '[JSON] (LomakeRequest UpdatePolicy)
        :> Post '[JSON] (CommandResponse ())
    -- Charts
    , routeChartPerUser :: route :-
        SSOUser :> "chart" :> "per-user" :> Get '[SVG] (Chart "per-user")
    , routeChartPerEndpoint :: route :-
        SSOUser :> "chart" :> "per-endpoint" :> Get '[SVG] (Chart "per-endpoint")
    , routeChartPerDay :: route :-
        SSOUser :> "chart" :> "per-day" :> Get '[SVG] (Chart "per-day")
    }
  deriving Generic

type ProxyMgmtAPI = ToServantApi ProxyMgmtRoutes

proxyMgmtApi :: Proxy ProxyMgmtAPI
proxyMgmtApi = genericApi (Proxy :: Proxy ProxyMgmtRoutes)
