{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.PlanMillSync.API where

import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant

import qualified FUM.Types.Login as FUM

type PlanMillSyncAPI =
    SSOUser :> Get '[HTML] (HtmlPage "index")
    -- actions
    :<|> AddDepartDateEndpoint
    :<|> UpdateStatusEndpoint
    :<|> UpdateContractTypeEndpoint

planmillSyncApi :: Proxy PlanMillSyncAPI
planmillSyncApi = Proxy

-------------------------------------------------------------------------------
-- Actions
-------------------------------------------------------------------------------

type AddDepartDateEndpoint = SSOUser :> "add-depart-date" :> Capture "login" FUM.Login :> Post '[JSON] (CommandResponse ())

addDepartDateEndpoint :: Proxy AddDepartDateEndpoint
addDepartDateEndpoint = Proxy

type UpdateStatusEndpoint = SSOUser :> "update-status" :> Capture "login" FUM.Login :> Post '[JSON] (CommandResponse ())

updateStatusEndpoint :: Proxy UpdateStatusEndpoint
updateStatusEndpoint = Proxy

type UpdateContractTypeEndpoint = SSOUser :> "update-contract-type" :> Capture "login" FUM.Login :> Post '[JSON] (CommandResponse ())

updateContractTypeEndpoint :: Proxy UpdateContractTypeEndpoint
updateContractTypeEndpoint = Proxy
