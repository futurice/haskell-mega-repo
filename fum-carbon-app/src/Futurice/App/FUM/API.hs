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

import qualified Personio

type FumCarbonApi = FumCarbonPagesApi
    -- reports
    :<|> "reports" :> "validations" :> Get '[HTML] (HtmlPage "validation-report")
    -- commands
    :<|> "commands" :> FumCarbonCommandApi
    -- machine api
    :<|> "api" :> FumCarbonMachineApi

type family FoldCommandAPI (cmds :: [Phase -> *]) :: * where
    FoldCommandAPI '[]           = EmptyAPI
    FoldCommandAPI (cmd ': cmds) = CommandEndpoint cmd :<|> FoldCommandAPI cmds

type FumCarbonCommandApi = FoldCommandAPI Commands

type FumCarbonMachineApi = FUMMachineAPI
    :<|> "personio-request" :> ReqBody '[JSON] Personio.SomePersonioReq :> Post '[JSON] Personio.SomePersonioRes
    :<|> "raw-employees" :> Get '[JSON] [Personio.Employee]
    :<|> "raw-employee-validations" :> Get '[JSON] [Personio.EmployeeValidation]

fumCarbonApi :: Proxy FumCarbonApi
fumCarbonApi = Proxy
