{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Aeson                              (object, (.=))
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

main' :: Logger -> OktaCfg -> Cmd -> IO ()
main' lgr token CmdGetAllUsers = do
    mgr <- liftIO $ newManager tlsManagerSettings
    users <- evalOktaReqIO token mgr lgr ReqGetAllUsers
    putPretty users
    pure ()
main' lgr token CmdGetAllGroups = do
    mgr <- liftIO $ newManager tlsManagerSettings
    groups <- evalOktaReqIO token mgr lgr ReqGetAllGroups
    putPretty groups
    pure ()
main' lgr token (CmdGetAllGroupMembers gid) = do
    mgr <- liftIO $ newManager tlsManagerSettings
    users <- evalOktaReqIO token mgr lgr $ ReqGetGroupUsers gid
    putPretty users
    pure ()
main' lgr token CmdGetAllApps = do
    mgr <- liftIO $ newManager tlsManagerSettings
    apps <- evalOktaReqIO token mgr lgr $ ReqGetAllApps
    putPretty apps
    pure ()
main' lgr token (CmdGetAppUsers aid) = do
    mgr <- liftIO $ newManager tlsManagerSettings
    users <- evalOktaReqIO token mgr lgr $ ReqGetAppUsers aid
    putPretty users
    pure ()

main :: IO ()
main = withStderrLogger $ \lgr -> runLogT "okta-cli" lgr $ do
    cfg <- getConfig "OKTACLIENT"
    liftIO $ (O.execParser opts >>= main' lgr cfg)
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
