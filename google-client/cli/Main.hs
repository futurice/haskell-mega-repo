{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Aeson                                (eitherDecodeStrict)
import Data.Text.Prettyprint.Doc                 (Doc, vcat, viaShow)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle, putDoc)
import Futurice.EnvConfig                        (getConfig)
import Futurice.Prelude
import Prelude ()
import Text.Read                                 (readEither)

import Google

import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE
import qualified Options.Applicative as O

data Cmd = CmdGetEvents Day Day Text
         | CmdGetResources
         | CmdSendInvite CalendarEvent
         | CmdDeleteEvent Text
         | CmdPatchEvent Text CalendarEvent

getEventOptions :: O.Parser Cmd
getEventOptions = CmdGetEvents
    <$> (O.argument (O.eitherReader readEither) $ mconcat
         [ O.metavar ":start-day"
         , O.help "Starting day (inclusive) from where to look for events"])
    <*> (O.argument (O.eitherReader readEither) $ mconcat
         [ O.metavar ":end-day"
         , O.help "End day (inclusive) from where to stop looking for events"])
    <*> strArgument [ O.metavar ":email", O.help "Email of room we want to get events"]

getResourceOptions :: O.Parser Cmd
getResourceOptions = pure CmdGetResources

readValue :: String -> Either String CalendarEvent
readValue s = case eitherDecodeStrict $ TE.encodeUtf8 $ T.pack s of
    Right v -> Right v
    Left err -> Left err

getInviteSendOptions :: O.Parser Cmd
getInviteSendOptions = CmdSendInvite
    <$> (O.argument (O.eitherReader readValue) $ mconcat
          [ O.metavar ":json"
          , O.help "Event data"
          ])

getDeleteEventOptions :: O.Parser Cmd
getDeleteEventOptions = CmdDeleteEvent <$> strArgument [ O.metavar ":event-id", O.help "Key value" ]

getPatchEventOptions :: O.Parser Cmd
getPatchEventOptions = CmdPatchEvent
    <$> strArgument [ O.metavar ":event-id", O.help "Key value" ]
    <*> (O.argument (O.eitherReader readValue) $ mconcat
          [ O.metavar ":json"
          , O.help "Event data"
          ])

strArgument :: IsString a => [O.Mod O.ArgumentFields a] -> O.Parser a
strArgument = O.strArgument . mconcat

optsParser :: O.Parser Cmd
optsParser = O.subparser $ mconcat
    [ cmdParser "get-events"    getEventOptions       "Get events"
    , cmdParser "get-resources" getResourceOptions    "Get resources"
    , cmdParser "send-invite"   getInviteSendOptions  "Send invites"
    , cmdParser "delete-event"  getDeleteEventOptions "Delete event"
    , cmdParser "patch-event"   getPatchEventOptions  "Patch event"
    ]
  where
    cmdParser :: String -> O.Parser Cmd -> String -> O.Mod O.CommandFields Cmd
    cmdParser cmd parser desc =
         O.command cmd $ O.info (O.helper <*> parser) $ O.progDesc desc

main' :: GoogleCredentials -> Cmd -> IO ()
main' cfg (CmdSendInvite calendarEvent) = do
    mgr <- liftIO $ newManager tlsManagerSettings
    event <- evalGoogleReqIO cfg mgr $ ReqInvite calendarEvent
    print event
    pure ()
main' cfg (CmdDeleteEvent eventId) = do
    mgr <- liftIO $ newManager tlsManagerSettings
    evalGoogleReqIO cfg mgr $ ReqDeleteEvent eventId
main' cfg CmdGetResources = do
    mgr <- liftIO $ newManager tlsManagerSettings
    resources <- evalGoogleReqIO cfg mgr $ ReqCalendarResources ReadOnly
    print resources
    pure ()
main' cfg (CmdGetEvents startDay endDay email) = do
    mgr <- liftIO $ newManager tlsManagerSettings
    events <- evalGoogleReqIO cfg mgr $ ReqEvents ReadOnly startDay endDay email
    putDoc $ toPretty events
    pure ()
main' cfg (CmdPatchEvent eventId eventData) = do
    mgr <- liftIO $ newManager tlsManagerSettings
    event <- evalGoogleReqIO cfg mgr $ ReqPatchEvent eventId eventData
    print event
    pure ()

main :: IO ()
main = withStderrLogger $ \lgr -> runLogT "google-cli" lgr $ do
    cfg <- getConfig "GOOGLECLIENT"
    liftIO $ (O.execParser opts >>= main' cfg)
  where
    opts = O.info (O.helper <*> optsParser) $ mconcat
        [ O.fullDesc
        , O.progDesc "Cli for google-client"
        , O.header "google-cli"
        ]

toPretty :: [Event] -> Doc AnsiStyle
toPretty events = vcat $ viaShow <$> events
