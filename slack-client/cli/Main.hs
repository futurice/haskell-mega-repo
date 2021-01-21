{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Futurice.EnvConfig                      (getConfig)
import Futurice.Prelude
import Prelude ()
import Text.PrettyPrint.ANSI.Leijen.AnsiPretty
       (AnsiPretty (..), linebreak, putDoc)

import Slack

import qualified Options.Applicative as O

data Cmd = CmdSendMessage Text Text
         | CmdGetUsers

getSendMessageOptions :: O.Parser Cmd
getSendMessageOptions = CmdSendMessage
    <$> strArgument [ O.metavar ":channel-id", O.help "Channel to send message" ]
    <*> strArgument [ O.metavar ":message", O.help "Message to send" ]

getUsersOptions :: O.Parser Cmd
getUsersOptions = pure CmdGetUsers

strArgument :: IsString a => [O.Mod O.ArgumentFields a] -> O.Parser a
strArgument = O.strArgument . mconcat

optsParser :: O.Parser Cmd
optsParser = O.subparser $ mconcat
    [ cmdParser "send-message" getSendMessageOptions "Send message"
    , cmdParser "get-users" getUsersOptions "Get users"
    ]
  where
    cmdParser :: String -> O.Parser Cmd -> String -> O.Mod O.CommandFields Cmd
    cmdParser cmd parser desc =
         O.command cmd $ O.info (O.helper <*> parser) $ O.progDesc desc

main' :: SlackToken -> Cmd -> IO ()
main' token (CmdSendMessage channelId message) = do
    mgr <- liftIO $ newManager tlsManagerSettings
    _ <- evalSlackReqIO token mgr $ ReqSendMessage (ChannelId channelId) message
    print "Message send successfully"
    pure ()
main' token CmdGetUsers = do
    mgr <- liftIO $ newManager tlsManagerSettings
    users <- evalSlackReqIO token mgr ReqGetUsers
    putPretty users
    pure ()

main :: IO ()
main = withStderrLogger $ \lgr -> runLogT "slack-cli" lgr $ do
    cfg <- getConfig "SLACKCLIENT"
    liftIO $ (O.execParser opts >>= main' cfg)
  where
    opts = O.info (O.helper <*> optsParser) $ mconcat
        [ O.fullDesc
        , O.progDesc "Cli for slack-client"
        , O.header "slack-cli"
        ]
-------------------------------------------------------------------------------
-- putPretty
-------------------------------------------------------------------------------

putPretty :: (MonadIO m, AnsiPretty a) => a -> m ()
putPretty = liftIO . putDoc . (<> linebreak) . ansiPretty
