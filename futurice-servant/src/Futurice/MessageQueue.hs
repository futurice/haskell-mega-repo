{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
-- | "MessageQueue"
--
-- Messages
--
-- * are sent to Amazon SNS (Simple Notification Service)
--
-- * are received from Amazon SQS (Simple Queue Service)
--
-- * routing mesh is configured in AWS console
--
module Futurice.MessageQueue (
    -- * Types
    MessageQueue,
    Message (..),
    -- * Functions
    createMessageQueue,
    publishMessage,
    ) where

import Control.Monad.Trans.Resource (ResourceT)
import Data.Aeson                   (FromJSON, ToJSON, encode, object, (.=))
import Data.Distributive            (Distributive (..))
import Data.Functor.Rep
       (Representable (..), collectRep, distributeRep)
import Futurice.Prelude
import Futurice.Services
import Prelude ()
import Text.Regex.Applicative.Text  (anySym, few, match, sym)

import qualified Control.Monad.Trans.AWS     as AWS
import qualified Data.DList                  as DList
import qualified Data.Map.Strict             as Map
import qualified Network.AWS.SNS.CreateTopic as AWS
import qualified Network.AWS.SNS.ListTopics  as AWS
import qualified Network.AWS.SNS.Publish     as AWS
import qualified Network.AWS.SNS.Types       as AWS

-------------------------------------------------------------------------------
-- Message Queue
-------------------------------------------------------------------------------

data MessageQueue = MessageQueue
    { mqPublishMessage :: Message -> IO ()
    }

-------------------------------------------------------------------------------
-- Message
-------------------------------------------------------------------------------

data Message
    = ServiceStarting Service
    | PersonioUpdated
  deriving (Eq, Show, Generic)

instance ToJSON Message
instance FromJSON Message

-------------------------------------------------------------------------------
-- Topics
-------------------------------------------------------------------------------

data Topic
    = TopicServiceStarting
    | TopicPersonioUpdated
  deriving (Eq, Ord, Enum, Bounded, Show)

data PerTopic a = PerTopic
    { perServiceStarting :: a
    , perPersonioUpdated :: a
    }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Distributive PerTopic where
    distribute = distributeRep
    collect    = collectRep

instance Representable PerTopic where
    type Rep PerTopic = Topic

    index p TopicServiceStarting = perServiceStarting p
    index p TopicPersonioUpdated = perPersonioUpdated p

    tabulate f = PerTopic
        { perServiceStarting = f TopicServiceStarting
        , perPersonioUpdated = f TopicPersonioUpdated
        }

messageTopic :: Message -> Topic
messageTopic ServiceStarting {} = TopicServiceStarting
messageTopic PersonioUpdated {} = TopicPersonioUpdated

-- | A subject of email
messageSubject :: Message -> Text
messageSubject (ServiceStarting service) =
    "Service starting: " <> serviceToText service
messageSubject msg = textShow (messageTopic msg) -- default

topicName :: Text -> Topic -> Text
topicName awsGroup TopicServiceStarting = awsGroup <> "-service-starting"
topicName awsGroup TopicPersonioUpdated = awsGroup <> "-personio-updated"

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

type M = LogT (AWS.AWST (ResourceT IO))

-- | Create Message Queue
--
-- This will create SNS topics and a SQS queue for a service if they don't exist.
--
createMessageQueue
    :: Logger
    -> Maybe AWS.Env  -- ^ if 'Nothing', message queue will be no-op
    -> Text           -- ^ AWS "Group"
    -> Service        -- ^ Service, so we know which queue to read
    -- TODO: callback!
    -> IO MessageQueue
createMessageQueue _ Nothing _ _ = do
    return MessageQueue
        { mqPublishMessage = \_ -> return () -- no-op
        }
createMessageQueue lgr (Just env) awsGroup _service =
    AWS.runResourceT $ AWS.runAWST env $ runLogT "create-message-queue" lgr $ do
        topics <- createTopics
        logInfo "SNS Topics" (map show $ toList topics)

        createQueue

        return MessageQueue
            { mqPublishMessage = makePublishMessage topics
            }
  where
    makePublishMessage :: PerTopic AWS.Topic -> Message -> IO ()
    makePublishMessage topics msg =
        AWS.runResourceT $ AWS.runAWST env $ runLogT "publish-message" lgr $ do
            r <- AWS.send $ AWS.publish (decodeUtf8Lenient (encode msg ^. strict))
                & AWS.pTopicARN .~ (awsTopic ^. AWS.tTopicARN)
                & AWS.pSubject  ?~ messageSubject msg

            logInfo "message sent" $ object
                [ "message-id"      .= (r ^. AWS.prsMessageId)
                , "response-status" .= (r ^. AWS.prsResponseStatus)
                , "subject"         .= textShow msgTopic
                ]
      where
        msgTopic = messageTopic msg
        awsTopic = index topics msgTopic

    createTopics :: M (PerTopic AWS.Topic)
    createTopics = do
        r <- listAllTopics

        -- classify by name (last part of ARN)
        let m :: Map Text AWS.Topic
            m = Map.fromList $ catMaybes $ r <&> \t -> do
                arn <- t ^. AWS.tTopicARN
                lastPart <- match (many anySym *> sym ':' *> few anySym) arn
                return (lastPart ^. packed, t)

        -- create PerTopic structure
        for (tabulate id) $ \t -> do
            let name = topicName awsGroup t
            case m ^? ix name of
                -- if topic is there use it
                Just t' -> return t'

                -- otherwise create new
                Nothing -> do
                    logInfo "Creating SNS Topic" name
                    r1 <- AWS.send $ AWS.createTopic name
                    return $ AWS.topic & AWS.tTopicARN .~ (r1 ^. AWS.ctrsTopicARN)

    listAllTopics :: M [AWS.Topic]
    listAllTopics = do
        r <- AWS.send AWS.listTopics
        go (DList.fromList (r ^. AWS.ltrsTopics)) (r ^. AWS.ltrsNextToken)
      where
        go acc Nothing = return (DList.toList acc)
        go acc (Just t) = do
            r <- AWS.send $ AWS.listTopics & AWS.ltNextToken ?~ t
            go (acc <> DList.fromList (r ^. AWS.ltrsTopics)) (r ^. AWS.ltrsNextToken)

    createQueue = return () -- TODO

publishMessage :: MessageQueue -> Message -> IO ()
publishMessage (MessageQueue pub) = pub
