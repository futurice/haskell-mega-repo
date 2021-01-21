{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
module Slack.Types where

import Data.Aeson
import Futurice.EnvConfig (Configure (..), FromEnvVar (..), envVar)
import Futurice.Prelude
import Prelude ()

import qualified Data.Text as T

newtype ChannelId = ChannelId Text deriving (Eq, Show, Hashable)

instance FromEnvVar ChannelId where
    fromEnvVar = Just . ChannelId . T.pack

class HasHttpManager a where
    httpManager :: Lens' a Manager

class HasSlackToken a where
    slackToken :: Lens' a SlackToken

data User = User
    { slackDisplayName :: !Text
    , slackRealName    :: !Text
    , slackImageUrl    :: !Text
    } deriving Show

instance FromJSON User where
    parseJSON = withObject "user" $ \o -> do
        profile <- o .: "profile"
        User
            <$> profile .: "display_name"
            <*> profile .: "real_name"
            <*> profile .: "image_512"

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
