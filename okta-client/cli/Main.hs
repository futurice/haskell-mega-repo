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

getAllUsersOptions :: O.Parser Cmd
getAllUsersOptions = pure CmdGetAllUsers

optsParser :: O.Parser Cmd
optsParser = O.subparser $ mconcat
    [ cmdParser "get-all-users" getAllUsersOptions "Get all users from Okta"
    ]
  where
    cmdParser :: String -> O.Parser Cmd -> String -> O.Mod O.CommandFields Cmd
    cmdParser cmd parser desc =
         O.command cmd $ O.info (O.helper <*> parser) $ O.progDesc desc

main' :: OktaCfg -> Cmd -> IO ()
main' token CmdGetAllUsers = do
    mgr <- liftIO $ newManager tlsManagerSettings
    users <- evalOktaReqIO token mgr ReqGetAllUsers
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
