{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.ProxyMgmt.API where

import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Prelude
import Futurice.Servant
import Futurice.Lomake
import Prelude ()
import Servant
import Servant.API.Generic
import Dashdo.Servant (DashdoAPI)

import Futurice.App.ProxyMgmt.Commands.AddEndpoint
import Futurice.App.ProxyMgmt.Commands.AddToken
import Futurice.App.ProxyMgmt.Commands.RemoveEndpoint

data ProxyMgmtRoutes route = ProxyMgmtRoutes
    { routeIndexPage :: route :-
        SSOUser :> Get '[HTML] (HtmlPage "index")
    , routeRegenerateOwnToken :: route :-
        SSOUser :> "regenerate-own-token" :> Post '[JSON] Text
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
    -- dashdo
    , routeDashdo :: route :- "dashdo" :> DashdoAPI
    }
  deriving Generic

type ProxyMgmtAPI = ToServantApi ProxyMgmtRoutes

proxyMgmtApi :: Proxy ProxyMgmtAPI
proxyMgmtApi = genericApi (Proxy :: Proxy ProxyMgmtRoutes)
