{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.Okta.API where

import Futurice.Email            (Email)
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant.API
import Servant.API.Generic
import Servant.HTML.Lucid        (HTML)

import qualified Data.Map as Map
import qualified GitHub   as GH

data Record route = Record
    { githubUsernames :: route :- "github-usernames" :> Get '[JSON] (Map.Map Email (Maybe (GH.Name GH.User)))
    } deriving (Generic)

type OktaSyncAPI = ToServantApi Record

oktaSyncApi :: Proxy OktaSyncAPI
oktaSyncApi = genericApi (Proxy :: Proxy Record)


data HtmlRecord route = HtmlRecord
    { indexPageGet :: route :- SSOUser :> Summary "index page" :> Get '[HTML] (HtmlPage "indexpage")
    } deriving (Generic)

type HtmlAPI = ToServantApi HtmlRecord

htmlApi :: Proxy HtmlAPI
htmlApi = genericApi (Proxy :: Proxy HtmlRecord)
