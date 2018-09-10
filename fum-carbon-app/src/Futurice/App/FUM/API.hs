{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}
module Futurice.App.FUM.API (
    module Futurice.App.FUM.API,
    module Futurice.App.FUM.API.Pages,
    ) where

import Futurice.Prelude
import Prelude ()

import Futurice.Lucid.Foundation (HtmlPage)
import Servant.API
import Servant.HTML.Lucid        (HTML)

import Futurice.App.FUM.API.Pages
import Futurice.App.FUM.Command
import Futurice.FUM.MachineAPI

type FumCarbonApi = FumCarbonPagesApi
    -- reports
    :<|> "reports" :> "compare-old-fum" :> Get '[HTML] (HtmlPage "compare-old-fum-report")
    -- commands
    :<|> "commands" :> FumCarbonCommandApi
    -- machine api
    :<|> "api" :> FumCarbonMachineApi

type family FoldCommandAPI (cmds :: [Phase -> *]) :: * where
    FoldCommandAPI '[]           = EmptyAPI
    FoldCommandAPI (cmd ': cmds) = CommandEndpoint cmd :<|> FoldCommandAPI cmds

type FumCarbonCommandApi = FoldCommandAPI Commands

type FumCarbonMachineApi = FUMMachineAPI

fumCarbonApi :: Proxy FumCarbonApi
fumCarbonApi = Proxy
