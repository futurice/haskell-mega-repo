{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Aeson
import Data.Char                               (isAlphaNum)
import Futurice.EnvConfig                      (getConfig)
import Futurice.Prelude
import Prelude ()
import Text.PrettyPrint.ANSI.Leijen.AnsiPretty
       (AnsiPretty (..), linebreak, putDoc)

import Okta.Eval
import Okta.Request
import Okta.Types

import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE
import qualified Options.Applicative as O

data Cmd = CmdGetAllUsers
         | CmdGetAllGroups
         | CmdGetAllGroupsWithType GroupType
         | CmdGetAllGroupMembers Text Bool
         | CmdGetAllApps
         | CmdGetAppUsers Text
         | CmdGetUserApps Text
         | CmdGetApplication Text
         | CmdUpdateUser Text Value
         | CmdGetSlackUsers Text

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

getApplicationOptions :: O.Parser Cmd
getApplicationOptions = CmdGetApplication <$> strArgument [ O.metavar ":app-id", O.help "App id"]

getSlackUsersOptions :: O.Parser Cmd
getSlackUsersOptions = CmdGetSlackUsers <$> strArgument [ O.metavar ":app-id", O.help "Slack app id"]

readValue :: String -> Either String Value
readValue s = case eitherDecodeStrict $ TE.encodeUtf8 t of
  Right v -> Right v
  Left err
      -- if it's not valid JSON, but looks like an identifier
      | T.all (\c -> isAlphaNum c || c == '-' || c == '_') t -> Right (String t)
      | otherwise -> Left err
  where
    t = T.pack s

getUpdateUserOptions :: O.Parser Cmd
getUpdateUserOptions = CmdUpdateUser <$> strArgument [ O.metavar ":user-id", O.help "User id"] <*> (O.argument (O.eitherReader readValue) $ mconcat
        [ O.metavar ":json"
        , O.help "Profile payload"
        ])

optsParser :: O.Parser Cmd
optsParser = O.subparser $ mconcat
    [ cmdParser "get-all-users"  getAllUsersOptions "Get all users"
    , cmdParser "get-all-groups" getAllGroupsOptions "Get all groups"
    , cmdParser "get-all-groups-with-type" getAllGroupsWithTypeOptions "Get all groups filtered with type"
    , cmdParser "get-group-members" getAllGroupMembersOptions "Get all members of spesific group"
    , cmdParser "get-all-apps" getAllAppsOptions "Get all apps"
    , cmdParser "get-app-users" getAppUsersOptions "Get all users assigned to app"
    , cmdParser "get-user-apps" getUserAppsOptions "Get all apps user has"
    , cmdParser "get-application" getApplicationOptions "Get application information"
    , cmdParser "update-user" getUpdateUserOptions "Update user information"
    , cmdParser "get-slack-users" getSlackUsersOptions "Get all Slack application users"
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
main' lgr token (CmdGetApplication aid) = do
    mgr <- liftIO $ newManager tlsManagerSettings
    users <- evalOktaReqIO token mgr lgr $ ReqGetApplication $ OktaAppId aid
    putPretty users
    pure ()
main' lgr token (CmdUpdateUser oid value) = do
    mgr <- liftIO $ newManager tlsManagerSettings
    user <- evalOktaReqIO token mgr lgr $ ReqUpdateUser (OktaId oid) value
    putPretty user
    pure ()
main' lgr token (CmdGetSlackUsers aid) = do
    mgr <- liftIO $ newManager tlsManagerSettings
    users <- evalOktaReqIO token mgr lgr $ ReqGetSlackUsers $ OktaAppId aid
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
