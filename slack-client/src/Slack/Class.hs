module Slack.Class where

import Futurice.Prelude
import Prelude ()

import Slack.Request
import Slack.Types

class Monad m => MonadSlack m where
    slackReq :: Req a -> m a

slackProfiles :: MonadSlack m => m [User]
slackProfiles = slackReq ReqGetUsers
