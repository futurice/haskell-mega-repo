{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Futurice.App.FUM.Command.Server (commandServer) where

import Futurice.Prelude
import Generics.SOP     (All, SList (..), SListI (..))
import Prelude ()
import Servant

import Futurice.App.FUM.API
import Futurice.App.FUM.Auth
import Futurice.App.FUM.Command
import Futurice.App.FUM.Ctx
import Futurice.App.FUM.Transactor
import Futurice.App.FUM.Types

class    (Command cmd, ICT cmd) => Command' cmd
instance (Command cmd, ICT cmd) => Command' cmd

cmdServer
    :: forall cmd. Command' cmd
    => Ctx -> Server (CommandEndpoint cmd)
cmdServer ctx mlogin (LomakeRequest cmdInput) = runLogT "command" (ctxLogger ctx) $
    withAuthUser' (error "lomake error") ctx mlogin $ \(AuthUser login rights) world _ -> do
        now <- currentTime
        cmdInternal' <- hoist liftIO $ runExceptT $
            runReaderT (internalizeCommand now login rights cmdInput) world
        case cmdInternal' of
            Left err -> pure (CommandResponseError err)
            Right cmdInternal -> do
                res <- hoist liftIO $ transact ctx now login (someCommand cmdInternal)
                case res of
                    Right res' -> pure res'
                    Left err   -> pure (CommandResponseError err)

commandServer :: Ctx -> Server FumCarbonCommandApi
commandServer ctx = cmdServers (Proxy :: Proxy Commands)
  where
    cmdServers
        :: forall cmds. All Command' cmds
        => Proxy cmds
        -> Server (FoldCommandAPI cmds)
    cmdServers p = case sList :: SList cmds of
        SNil  -> emptyServer
        SCons -> cmdServers' p

    cmdServers'
        :: forall cmd cmds. (Command' cmd, All Command' cmds)
        => Proxy (cmd ': cmds)
        -> Server (CommandEndpoint cmd :<|> FoldCommandAPI cmds)
    cmdServers' _ = cmdServer ctx :<|> cmdServers (Proxy :: Proxy cmds)
