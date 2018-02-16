module Futurice.App.FUM.Auth where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Lens           (has)
import Futurice.Prelude
import Futurice.Exit
import Prelude ()
import Servant                (Handler)

import Futurice.App.FUM.ACL
import Futurice.App.FUM.Ctx
import Futurice.App.FUM.Types

import qualified Futurice.IdMap as IdMap
import qualified Personio

-- | Wrap endpoint requiring authentication.
withAuthUser'
    :: a  -- ^ Response to unauthenticated users
    -> Ctx
    -> Maybe Login
    -> (AuthUser -> World -> IdMap.IdMap Personio.Employee -> LogT Handler a)
    -> LogT Handler a
withAuthUser' def ctx mfu f = case mfu <|> ctxMockUser ctx of
    Nothing -> pure def
    Just fu -> do
        (world, es) <- liftIO $ atomically $ (,)
             <$> readTVar (ctxWorld ctx)
             <*> readTVar (ctxPersonio ctx)

        let rights = runExit $ do
                when (isSudoer fu world) $ exit RightsIT
                when (has (worldEmployees . ix fu) world) $ exit RightsNormal
                return RightsOther

        f (AuthUser fu rights) world es
