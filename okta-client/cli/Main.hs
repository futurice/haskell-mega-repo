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

import qualified Data.Text           as T
import qualified Options.Applicative as O

data Cmd = CmdGetAllUsers
         | CmdGetAllGroups
         | CmdGetAllGroupsWithType GroupType
         | CmdGetAllGroupMembers Text Bool
         | CmdGetAllApps
         | CmdGetAppUsers Text
         | CmdGetUserApps Text

getAllUsersOptions :: O.Parser Cmd
getAllUsersOptions = pure CmdGetAllUsers

getAllGroupsOptions :: O.Parser Cmd
getAllGroupsOptions = pure CmdGetAllGroups

getAllGroupsWithTypeOptions :: O.Parser Cmd
getAllGroupsWithTypeOptions = CmdGetAllGroupsWithType <$> argument (O.maybeReader (groupFromText . T.pack)) [ O.metavar ":group-type", O.help "Group type"]

getAllGroupMembersOptions :: O.Parser Cmd
getAllGroupMembersOptions = CmdGetAllGroupMembers
    <$> strArgument [ O.metavar ":group-id", O.help "Group id"]
    <*> O.switch (mconcat [ O.long "show-all"])

getAllAppsOptions :: O.Parser Cmd
getAllAppsOptions = pure CmdGetAllApps

getAppUsersOptions :: O.Parser Cmd
getAppUsersOptions = CmdGetAppUsers <$> strArgument [ O.metavar ":app-id", O.help "App id"]

getUserAppsOptions :: O.Parser Cmd
getUserAppsOptions = CmdGetUserApps <$> strArgument [ O.metavar ":user-id", O.help "User id"]

optsParser :: O.Parser Cmd
optsParser = O.subparser $ mconcat
    [ cmdParser "get-all-users"  getAllUsersOptions "Get all users"
    , cmdParser "get-all-groups" getAllGroupsOptions "Get all groups"
    , cmdParser "get-all-groups-with-type" getAllGroupsWithTypeOptions "Get all groups filtered with type"
    , cmdParser "get-group-members" getAllGroupMembersOptions "Get all members of spesific group"
    , cmdParser "get-all-apps" getAllAppsOptions "Get all apps"
    , cmdParser "get-app-users" getAppUsersOptions "Get all users assigned to app"
    , cmdParser "get-user-apps" getUserAppsOptions "get all apps user has"
    ]
  where
    cmdParser :: String -> O.Parser Cmd -> String -> O.Mod O.CommandFields Cmd
    cmdParser cmd parser desc =
         O.command cmd $ O.info (O.helper <*> parser) $ O.progDesc desc

strArgument :: IsString a => [O.Mod O.ArgumentFields a] -> O.Parser a
strArgument = O.strArgument . mconcat

argument :: O.ReadM a -> [O.Mod O.ArgumentFields a] -> O.Parser a
argument x y = O.argument x $ mconcat y

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
main' lgr token (CmdGetAllGroupsWithType groupFilter) = do
    mgr <- liftIO $ newManager tlsManagerSettings
    groups <- evalOktaReqIO token mgr lgr ReqGetAllGroups
    let groups' = filter (\g -> (groupType g) == groupFilter) groups
    putPretty groups'
    pure ()
main' lgr token (CmdGetAllGroupMembers gid showAll) = do
    mgr <- liftIO $ newManager tlsManagerSettings
    users <- evalOktaReqIO token mgr lgr $ ReqGetGroupUsers $ OktaGroupId gid
    let x = simpleUser <$> users
    putPretty $ if showAll
        then x
        else take 10 x
    pure ()
main' lgr token CmdGetAllApps = do
    mgr <- liftIO $ newManager tlsManagerSettings
    apps <- evalOktaReqIO token mgr lgr $ ReqGetAllApps
    putPretty apps
    pure ()
main' lgr token (CmdGetAppUsers aid) = do
    mgr <- liftIO $ newManager tlsManagerSettings
    users <- evalOktaReqIO token mgr lgr $ ReqGetAppUsers $ OktaAppId aid
    putPretty users
    pure ()
main' lgr token (CmdGetUserApps uid) = do
    mgr <- liftIO $ newManager tlsManagerSettings
    users <- evalOktaReqIO token mgr lgr $ ReqGetAppLinks $ OktaId uid
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
