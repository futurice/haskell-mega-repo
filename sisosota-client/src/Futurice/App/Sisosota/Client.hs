module Futurice.App.Sisosota.Client (
    sisosotaGet,
    sisosotaPut,
    ) where

import Futurice.Prelude
import Prelude ()
import Servant.Client
import Servant.Client.Generic

import Futurice.App.Sisosota.API
import Futurice.App.Sisosota.Types

cli :: Record (AsClientT ClientM)
cli = genericClient

sisosotaGet :: Manager -> BaseUrl -> ContentHash -> IO (Either String LazyByteString)
sisosotaGet mgr burl h = do
    e <- runClientM (recGet cli h) (mkClientEnv mgr burl)
    case e of
        Right (ContentData lbs) -> return (Right lbs)
        Left _err               -> return (Left "Not found")

sisosotaPut :: Manager -> BaseUrl -> LazyByteString -> IO ContentHash
sisosotaPut mgr burl lbs = do
    e <- runClientM (recPut cli (ContentData lbs)) (mkClientEnv mgr burl)
    either throwM  pure e
