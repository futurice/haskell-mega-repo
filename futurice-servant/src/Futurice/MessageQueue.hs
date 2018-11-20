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
    Subscription,
    Message (..),
    -- * Functions
    createMessageQueue,
    publishMessage,
    subscribeToQueue,
    readSubscription,
    forEachMessage,
    ) where

import Control.Concurrent           (ThreadId, forkIO)
import Control.Lens                 (firstOf, forOf_)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Distributive            (Distributive (..))
import Data.Functor.Rep
       (Representable (..), collectRep, distributeRep)
import Futurice.Aeson
       (FromJSON (..), ToJSON, eitherDecodeStrict, encode, object,
       withObjectDump, (.:), (.=))
import Futurice.Prelude
import Futurice.Services
import Prelude ()
import Text.Regex.Applicative.Text  (anySym, few, match, sym)

import qualified Control.Concurrent.STM         as STM
import qualified Control.Monad.Trans.AWS        as AWS
import qualified Data.DList                     as DList
import qualified Data.Map.Strict                as Map
import qualified Network.AWS.SNS.CreateTopic    as AWS
import qualified Network.AWS.SNS.ListTopics     as AWS
import qualified Network.AWS.SNS.Publish        as AWS
import qualified Network.AWS.SNS.Types          as AWS
import qualified Network.AWS.SQS.CreateQueue    as AWS
import qualified Network.AWS.SQS.DeleteMessage  as AWS
import qualified Network.AWS.SQS.ListQueues     as AWS
import qualified Network.AWS.SQS.ReceiveMessage as AWS
import qualified Network.AWS.SQS.Types          as AWS

-------------------------------------------------------------------------------
-- Message Queue
-------------------------------------------------------------------------------

data MessageQueue = MQ
    { mqPublishMessage :: !(Message -> IO ())
    , mqSubscribe      :: !(Maybe (STM.STM Subscription))
    }

newtype Subscription = Sub (STM.STM Message)

-------------------------------------------------------------------------------
-- Message
-------------------------------------------------------------------------------

data Message
    = ServiceStarting Service
    | PersonioUpdated
    | MissingHoursPing
    | LibraryReminderPing
  deriving (Eq, Show, Generic)

instance ToJSON Message
instance FromJSON Message

-------------------------------------------------------------------------------
-- Topics
-------------------------------------------------------------------------------

data Topic
    = TopicServiceStarting
    | TopicPersonioUpdated
    | TopicMissingHoursPing
    | TopicLibraryReminderPing
  deriving (Eq, Ord, Enum, Bounded, Show)

data PerTopic a = PerTopic
    { perServiceStarting     :: a
    , perPersonioUpdated     :: a
    , perMissingHoursPing    :: a
    , perLibraryReminderPing :: a
    }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Distributive PerTopic where
    distribute = distributeRep
    collect    = collectRep

instance Representable PerTopic where
    type Rep PerTopic = Topic

    index p TopicServiceStarting     = perServiceStarting p
    index p TopicPersonioUpdated     = perPersonioUpdated p
    index p TopicMissingHoursPing    = perMissingHoursPing p
    index p TopicLibraryReminderPing = perLibraryReminderPing p

    tabulate f = PerTopic
        { perServiceStarting     = f TopicServiceStarting
        , perPersonioUpdated     = f TopicPersonioUpdated
        , perMissingHoursPing    = f TopicMissingHoursPing
        , perLibraryReminderPing = f TopicLibraryReminderPing
        }

messageTopic :: Message -> Topic
messageTopic ServiceStarting {}     = TopicServiceStarting
messageTopic PersonioUpdated {}     = TopicPersonioUpdated
messageTopic MissingHoursPing {}    = TopicMissingHoursPing
messageTopic LibraryReminderPing {} = TopicLibraryReminderPing

-- | A subject of email
messageSubject :: Message -> Text
messageSubject (ServiceStarting service) =
    "Service starting: " <> serviceToText service
messageSubject msg = textShow (messageTopic msg) -- default

topicName :: Text -> Topic -> Text
topicName awsGroup TopicServiceStarting     = awsGroup <> "-service-starting"
topicName awsGroup TopicPersonioUpdated     = awsGroup <> "-personio-updated"
topicName awsGroup TopicMissingHoursPing    = awsGroup <> "-missing-hours-ping"
topicName awsGroup TopicLibraryReminderPing = awsGroup <> "-library-reminder-ping"

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
    -> Maybe AWS.Env  -- ^ if 'Nothing', message queue will be no-op: publish won't do anything, subscribe will 'STM.retry'.
    -> Text           -- ^ AWS "Group"
    -> Service        -- ^ Service, so we know which queue to read
    -> IO MessageQueue
createMessageQueue _ Nothing _ _ = do

    return MQ
        { mqPublishMessage = \_ -> return () -- no-op
        , mqSubscribe      = Nothing
        }
createMessageQueue lgr (Just env) awsGroup service =
    AWS.runResourceT $ AWS.runAWST env $ runLogT "create-message-queue" lgr $ do
        topics <- createTopics
        logInfo "SNS Topics" (map show $ toList topics)

        queueUrl <- createQueue
        logInfo "SQS Queue" queueUrl
        subscribe <- liftIO $ makeSubscribe queueUrl

        return MQ
            { mqPublishMessage = makePublishMessage topics
            , mqSubscribe      = Just subscribe
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

    queueName = awsGroup <> "-" <> serviceToText service

    createQueue :: M Text
    createQueue = do
        r <- AWS.send $ AWS.listQueues & AWS.lqQueueNamePrefix ?~ queueName
        case firstOf (AWS.lqrsQueueURLs . folded) r of
            Just qUrl -> return qUrl
            Nothing   -> do
                logInfo "Creating SQS Queue" queueName
                r1 <- AWS.send $ AWS.createQueue queueName
                case r1 ^. AWS.cqrsQueueURL of
                    Nothing -> do
                        logAttention "Queue creation failed" (show r1)
                        fail "Queue creation failed"
                    Just n  -> return n

    makeSubscribe :: Text -> IO (STM.STM Subscription)
    makeSubscribe queueUrl = do
        chan <- STM.newBroadcastTChanIO
        let subscription = do
                r <- STM.dupTChan chan
                return (Sub (STM.readTChan r))

        -- TODO: tryDeep log here too
        _threadId <- forkIO $ forever $ tryDeep $ AWS.runResourceT $ AWS.runAWST env $ runLogT "receive-message" lgr $ do
            r <- AWS.send $ AWS.receiveMessage queueUrl
                & AWS.rmWaitTimeSeconds ?~ 10
            forOf_ (AWS.rmrsMessages . folded) r $ \msg -> do
                logTrace "received message" (show msg)

                -- Delete message right away
                for_ (msg ^. AWS.mReceiptHandle) $ \receiptHandle -> do
                    _r1 <- AWS.send $ AWS.deleteMessage queueUrl receiptHandle
                    -- logTrace "deleted message" (show r1)
                    return ()

                -- process message
                for_ (msg ^. AWS.mBody) $ \body ->
                    case eitherDecodeStrict (encodeUtf8 body) of
                        -- Unknown message
                        Left err -> logAttention "Invalid message received" $ object
                            [ "error" .= err
                            , "body"  .= body
                            ]

                        -- On success write to broadcast chan
                        Right (Env msg') -> liftIO $ STM.atomically $ STM.writeTChan chan msg'

        return subscription

newtype Env = Env Message

instance FromJSON Env where
    parseJSON = withObjectDump "Notification" $ \obj -> do
        contents <- obj .: "Message"
        either fail (return . Env) $ eitherDecodeStrict (encodeUtf8 contents)

publishMessage :: MessageQueue -> Message -> IO ()
publishMessage (MQ pub _) = pub

subscribeToQueue :: MessageQueue -> Maybe (STM.STM Subscription)
subscribeToQueue (MQ _ subscribe) = subscribe

readSubscription :: Subscription -> STM.STM Message
readSubscription (Sub s) = s

-- | High-level helper to process messages.
--
-- 'forEachMessage' forks a forever running thread which tries to read
-- from a message queue.
--
-- Returns nothing if there are no MessageQueue.
--
forEachMessage :: MessageQueue -> (Message -> IO ()) -> IO (Maybe ThreadId)
forEachMessage mq action = for (subscribeToQueue mq) $ \s' -> do
    s <- STM.atomically s'
 -- TODO: log exceptions? change futuriceServerMain to take LogT IO
    forkIO $ forever $ tryDeep $ do
        msg <- STM.atomically $ readSubscription s
        action msg
