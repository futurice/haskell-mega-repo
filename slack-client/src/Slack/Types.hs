{-# LANGUAGE GADTs #-}
module Slack.Types where

import Futurice.EnvConfig (Configure (..), FromEnvVar (..), envVar)
import Futurice.Prelude
import Prelude ()

import qualified Data.Text as T

newtype ChannelId = ChannelId Text

instance FromEnvVar ChannelId where
    fromEnvVar = Just . ChannelId . T.pack

class HasHttpManager a where
    httpManager :: Lens' a Manager

class HasSlackToken a where
    slackToken :: Lens' a SlackToken

data Req a where
    ReqSendMessage :: ChannelId -> Text -> Req ()

newtype SlackToken = SlackToken Text

instance Configure SlackToken where
    configure = SlackToken <$> envVar "SLACK_TOKEN"

data Cfg = Cfg
    { slackCfg   :: !SlackToken
    , manager    :: !Manager
    }

instance HasHttpManager Cfg where
    httpManager f cfg = fmap (\newm -> cfg { manager = newm }) (f (manager cfg))

instance HasSlackToken Cfg where
    slackToken f cfg = fmap (\newToken -> cfg { slackCfg = newToken}) (f (slackCfg cfg))

newtype SlackError = SlackError String deriving Show

instance Exception SlackError