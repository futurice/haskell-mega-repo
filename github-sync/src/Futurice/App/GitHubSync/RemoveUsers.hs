{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.GitHubSync.RemoveUsers where

import Data.Aeson        (object, (.=))
import Futurice.Postgres
import Futurice.Prelude
import Prelude ()

import qualified FUM.Types.Login as FUM
import qualified GitHub          as GH

import Futurice.App.GitHubSync.Config
import Futurice.App.GitHubSync.Ctx

removeUsers :: Ctx -> FUM.Login -> [GH.Name GH.User] -> IO Bool
removeUsers ctx login us = runLogT "remove-users" lgr $ do
    -- audit log
    n <- safePoolExecute ctx
        "INSERT INTO \"github-sync\".auditlog (login, timestamp, action) VALUES (?, now(), ?)"
        (login, object [ "command" .= ("remove-users" :: Text), "users" .= us ])

    if n == 0
    then return False
    else do
        -- action
        vs <- for us $ \u -> do
            -- https://developer.github.com/v3/orgs/members/#remove-organization-membership
            let req = GH.command GH.Delete
                    [ "orgs", GH.toPathPart $ cfgOrganisationName $ ctxConfig ctx, "memberships", GH.toPathPart u]
                    mempty
            logTrace "executing" (show req)
            r <- liftIO $ GH.executeRequestWithMgr mgr auth req
            case r of
                Right _ -> return True
                Left  _ -> return False

        -- TODO: better "command handling"
        return (and vs)
  where
    cfg = ctxConfig ctx
    mgr = ctxManager ctx
    lgr = ctxLogger ctx
    auth = cfgAuth cfg
