{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Futurice.EnvConfig                      (getConfig)
import Futurice.Prelude
import Prelude ()
import Text.PrettyPrint.ANSI.Leijen.AnsiPretty
       (AnsiPretty (..), linebreak, putDoc)

import Okta.Eval
import Okta.Request
import Okta.Types

import qualified Options.Applicative as O

data Cmd = CmdGetAllUsers
         | CmdGetAllGroups
         | CmdGetAllGroupMembers Text
         | CmdGetAllApps
         | CmdGetAppUsers Text

getAllUsersOptions :: O.Parser Cmd
getAllUsersOptions = pure CmdGetAllUsers

getAllGroupsOptions :: O.Parser Cmd
getAllGroupsOptions = pure CmdGetAllGroups

getAllGroupMembersOptions :: O.Parser Cmd
getAllGroupMembersOptions = CmdGetAllGroupMembers <$> strArgument [ O.metavar ":group-id", O.help "Group id"]

getAllAppsOptions :: O.Parser Cmd
getAllAppsOptions = pure CmdGetAllApps

getAppUsersOptions :: O.Parser Cmd
getAppUsersOptions = CmdGetAppUsers <$> strArgument [ O.metavar ":app-id", O.help "App id"]

optsParser :: O.Parser Cmd
optsParser = O.subparser $ mconcat
    [ cmdParser "get-all-users"  getAllUsersOptions "Get all users"
    , cmdParser "get-all-groups" getAllGroupsOptions "Get all groups"
    , cmdParser "get-group-members" getAllGroupMembersOptions "Get all members of spesific group"
    , cmdParser "get-all-apps" getAllAppsOptions "Get all apps"
    , cmdParser "get-app-users" getAppUsersOptions "Get all users assigned to app"
    ]
  where
    cmdParser :: String -> O.Parser Cmd -> String -> O.Mod O.CommandFields Cmd
    cmdParser cmd parser desc =
         O.command cmd $ O.info (O.helper <*> parser) $ O.progDesc desc

strArgument :: IsString a => [O.Mod O.ArgumentFields a] -> O.Parser a
strArgument = O.strArgument . mconcat

main' :: OktaCfg -> Cmd -> IO ()
main' token CmdGetAllUsers = do
    mgr <- liftIO $ newManager tlsManagerSettings
    users <- evalOktaReqIO token mgr ReqGetAllUsers
    putPretty users
    pure ()
main' token CmdGetAllGroups = do
    mgr <- liftIO $ newManager tlsManagerSettings
    groups <- evalOktaReqIO token mgr ReqGetAllGroups
    putPretty groups
    pure ()
main' token (CmdGetAllGroupMembers gid) = do
    mgr <- liftIO $ newManager tlsManagerSettings
    users <- evalOktaReqIO token mgr $ ReqGetGroupUsers gid
    putPretty users
    pure ()
main' token CmdGetAllApps = do
    mgr <- liftIO $ newManager tlsManagerSettings
    apps <- evalOktaReqIO token mgr $ ReqGetAllApps
    putPretty apps
    pure ()
main' token (CmdGetAppUsers aid) = do
    mgr <- liftIO $ newManager tlsManagerSettings
    users <- evalOktaReqIO token mgr $ ReqGetAppUsers aid
    putPretty users
    pure ()

main :: IO ()
main = withStderrLogger $ \lgr -> runLogT "okta-cli" lgr $ do
    cfg <- getConfig "OKTACLIENT"
    liftIO $ (O.execParser opts >>= main' cfg)
  where
    opts = O.info (O.helper <*> optsParser) $ mconcat
        [ O.fullDesc
        , O.progDesc "Cli for okta-client"
        , O.header "okta-cli"
        ]

-------------------------------------------------------------------------------
-- putPretty
-------------------------------------------------------------------------------

putPretty :: (MonadIO m, AnsiPretty a) => a -> m ()
putPretty = liftIO . putDoc . (<> linebreak) . ansiPretty
