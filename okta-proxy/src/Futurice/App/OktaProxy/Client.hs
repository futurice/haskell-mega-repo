module Futurice.App.OktaProxy.Client where

import FUM.Types.Login        (Login)
import Futurice.Prelude
import Prelude ()
import Servant.Client
import Servant.Client.Generic

import Futurice.App.OktaProxy.API

cli :: Record (AsClientT ClientM)
cli = genericClient

groupMembers :: Manager -> BaseUrl -> Text -> IO [Login]
groupMembers mgr burl groupName = do
    res <- runClientM (getGroupMembers cli groupName) (mkClientEnv mgr burl)
    either throwM pure res
