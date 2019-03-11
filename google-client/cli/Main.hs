{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Futurice.EnvConfig (getConfig)
import Futurice.Prelude
import Prelude ()

import Google.Types

import qualified Options.Applicative as O

data Cmd = CmdGetEvents
         | CmdResourceOptions

getEventOptions :: O.Parser Cmd
getEventOptions = pure CmdGetEvents

getResourceOptions :: O.Parser Cmd
getResourceOptions = pure CmdResourceOptions

optsParser :: O.Parser Cmd
optsParser = O.subparser $ mconcat
    [ cmdParser "get-events"    getEventOptions     "Get events"
    , cmdParser "get-resources" getResourceOptions  "Get resources"
    ]
  where
    cmdParser :: String -> O.Parser Cmd -> String -> O.Mod O.CommandFields Cmd
    cmdParser cmd parser desc =
         O.command cmd $ O.info (O.helper <*> parser) $ O.progDesc desc

main' :: Cmd -> IO ()
main' CmdGetEvents = print "We got optio"

main :: IO ()
main = withStderrLogger $ \lgr -> runLogT "google-cli" lgr $ do
    cfg <- getConfig "GOOGLECLIENT"
    liftIO $ print (cfg :: GoogleCredentials)
    liftIO $ (O.execParser opts >>= main')
  where
    opts = O.info (O.helper <*> optsParser) $ mconcat
        [ O.fullDesc
        , O.progDesc "Cli for google-client"
        , O.header "google-cli"
        ]
