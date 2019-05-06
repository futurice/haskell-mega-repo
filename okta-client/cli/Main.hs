{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Futurice.EnvConfig (getConfig)
import Futurice.Prelude
import Prelude ()

import Okta.Eval
import Okta.Request
import Okta.Types

import qualified Options.Applicative as O

data Cmd = CmdGetAllUsers
         | CmdGetAllGroups
         | GmdGetAllGroupMembers Text

getAllUsersOptions :: O.Parser Cmd
getAllUsersOptions = pure CmdGetAllUsers

getAllGroupsOptions :: O.Parser Cmd
getAllGroupsOptions = pure CmdGetAllGroups

getAllGroupMembersOptions :: O.Parser Cmd
getAllGroupMembersOptions = GmdGetAllGroupMembers <$> strArgument [ O.metavar ":group-id", O.help "Group id"]

optsParser :: O.Parser Cmd
optsParser = O.subparser $ mconcat
    [ cmdParser "get-all-users"  getAllUsersOptions "Get all users"
    , cmdParser "get-all-groups" getAllGroupsOptions "Get all groups"
    , cmdParser "get-group-members" getAllGroupMembersOptions "Get all members of spesific group"
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
    print users
    pure ()
main' token CmdGetAllGroups = do
    mgr <- liftIO $ newManager tlsManagerSettings
    users <- evalOktaReqIO token mgr ReqGetAllGroups
    print users
    pure ()
main' token (GmdGetAllGroupMembers groupId) = do
    mgr <- liftIO $ newManager tlsManagerSettings
    users <- evalOktaReqIO token mgr $ ReqGetGroupUsers groupId
    print users
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
