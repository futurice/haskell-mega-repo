{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Aeson         (Value (Null, String), eitherDecodeStrict)
import Futurice.EnvConfig (getConfig)
import Futurice.Prelude
import Prelude ()

import Google

import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE
import qualified Options.Applicative as O

data Cmd = CmdGetEvents
         | CmdResourceOptions
         | CmdSendInvite Text CalendarEvent
         | CmdDeleteEvent Text

getEventOptions :: O.Parser Cmd
getEventOptions = pure CmdGetEvents

getResourceOptions :: O.Parser Cmd
getResourceOptions = pure CmdResourceOptions

getInviteSendOptions :: O.Parser Cmd
getInviteSendOptions = CmdSendInvite
    <$> strArgument [ O.metavar ":attendee", O.help "Attendee email" ]
    <*> (O.argument (O.eitherReader readValue) $ mconcat
          [ O.metavar ":json"
          , O.help "Event data"
          ])
  where
    readValue :: String -> Either String CalendarEvent
    readValue s = case eitherDecodeStrict $ TE.encodeUtf8 $ T.pack s of
        Right v -> Right v
        Left err -> Left err

getDeleteEventOptions :: O.Parser Cmd
getDeleteEventOptions = CmdDeleteEvent <$> strArgument [ O.metavar ":event-id", O.help "Key value" ]

strArgument :: IsString a => [O.Mod O.ArgumentFields a] -> O.Parser a
strArgument = O.strArgument . mconcat

optsParser :: O.Parser Cmd
optsParser = O.subparser $ mconcat
    [ cmdParser "get-events"    getEventOptions       "Get events"
    , cmdParser "get-resources" getResourceOptions    "Get resources"
    , cmdParser "send-invite"   getInviteSendOptions  "Send invites"
    , cmdParser "delete-event"  getDeleteEventOptions "Delete event"
    ]
  where
    cmdParser :: String -> O.Parser Cmd -> String -> O.Mod O.CommandFields Cmd
    cmdParser cmd parser desc =
         O.command cmd $ O.info (O.helper <*> parser) $ O.progDesc desc

main' :: GoogleCredentials -> Cmd -> IO ()
main' cfg (CmdSendInvite attendee calendarEvent) = do
    mgr <- liftIO $ newManager tlsManagerSettings
    event <- evalGoogleReqIO cfg mgr $ ReqInvite [attendee] calendarEvent
    print event
    pure ()
main' cfg (CmdDeleteEvent eventId) = do
    mgr <- liftIO $ newManager tlsManagerSettings
    evalGoogleReqIO cfg mgr $ ReqDeleteEvent eventId
main' _ _ = print "We got optio"

main :: IO ()
main = withStderrLogger $ \lgr -> runLogT "google-cli" lgr $ do
    cfg <- getConfig "GOOGLECLIENT"
    liftIO $ print (cfg :: GoogleCredentials)
    liftIO $ (O.execParser opts >>= main' cfg)
  where
    opts = O.info (O.helper <*> optsParser) $ mconcat
        [ O.fullDesc
        , O.progDesc "Cli for google-client"
        , O.header "google-cli"
        ]
