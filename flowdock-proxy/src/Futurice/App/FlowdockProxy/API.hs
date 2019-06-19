{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.FlowdockProxy.API where

import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant
import Servant.API.Generic
import Servant.Cached            (CACHED, Cached)
import Servant.Chart             (Chart, SVG)

import qualified Chat.Flowdock.REST as FD

import Futurice.App.FlowdockProxy.DB (Row)

data Record route = Record
    { recIndex :: route
        :- QueryParam "needle" Text
        :> QueryParam "nick" Text
        :> QueryParam "flow" FD.FlowId
        :> Get '[HTML] (HtmlPage "index-page")
    , recCharts :: route
        :- "charts"
        :> Get '[HTML] (HtmlPage "charts")
    , recChartActivity :: route
        :- "charts"
        :> "activity"
        :> Get '[CACHED SVG] (Cached SVG (Chart "activity"))

    -- API
    , recApiFlows :: route
        :- Summary "Available (= proxied) flows"
        :> "api" :> "flows"
        :> Get '[JSON] [FD.FlowId]

    , recApiMessages :: route
        :- Summary "Get flow messages"
        :> "api" :> "flows"
        :> Capture "flow" FD.FlowId
        :> "messages"
        :> Summary "Get row messages"
        :> StreamGet JSONFraming JSON (SourceIO Row)
    }
  deriving Generic

type FlowdockProxyAPI = ToServantApi Record

flowdockProxyApi :: Proxy FlowdockProxyAPI
flowdockProxyApi = genericApi (Proxy :: Proxy Record)
