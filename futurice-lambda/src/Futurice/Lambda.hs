{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.Lambda (
    makeAwsLambda,
    AwsLambdaHandler,
    -- * LambdaContext
    LambdaContext,
    lcRemainingTimeInMillis,
    -- * Re-exports
    AWSEnv,
    ) where

import AWS.Lambda.RuntimeAPI
       (GenRequest (..), Response (..), defaultMain, getTimeRemaining,
       jsonResponse, makeMockRequest)
import Control.Exception          (displayException)
import Data.Aeson
       (FromJSON, ToJSON, Value, eitherDecodeStrict, encode, object, (.=))
import Data.Aeson.Types           (emptyObject)
import Data.Char                  (toUpper)
import Foreign.C
       (CLong (..), CString, peekCString, withCString)
import Foreign.Ptr                (Ptr)
import Futurice.EnvConfig         (Configure, getConfig)
import Futurice.Metrics.RateMeter (values)
import Futurice.Prelude
import GHC.Stats                  (getRTSStats)
import Log                        (LogMessage (..), LogT, runLogT, showLogLevel)
import Log.Internal.Logger        (Logger (..))
import Log.Monad                  (LogT (..))
import Prelude ()
import System.Environment         (lookupEnv, getArgs)
import System.IO                  (hFlush, stdout)

import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Char8                as BS8
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.Map                             as Map
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as TE
import qualified Network.AWS                          as AWS
import qualified Network.AWS.CloudWatch.PutMetricData as AWS
import qualified Network.AWS.CloudWatch.Types         as AWS
import qualified Network.AWS.Env                      as AWS

type AWSEnv = AWS.Env

-- | Essentially: get remaining time
type LambdaContext = IO Int64
type AwsLambdaHandler = IO ()

lcRemainingTimeInMillis :: IO Int64 -> IO Int
lcRemainingTimeInMillis = fmap fromIntegral

-- | Make an AWS Lambda handler.
makeAwsLambda
    :: (FromJSON a, ToJSON b, NFData b, Configure cfg)
    => (LambdaContext -> AWS.Env -> cfg -> Logger -> Manager -> a -> LogT IO b)
    -> AwsLambdaHandler
makeAwsLambda handler = do
    e <- lookupEnv "AWS_LAMBDA_RUNTIME_API"
    case e of
        Just _  -> awsLambda handler
        Nothing ->  localLambda handler

localLambda
    :: (FromJSON a, ToJSON b, NFData b, Configure cfg)
    => (LambdaContext -> AWS.Env -> cfg -> Logger -> Manager -> a -> LogT IO b)
    -> IO ()
localLambda handler = do
    let fnName = "LAMBDA"
    let fnNameT = fnName ^. packed

    args <- getArgs
    let (timeout, input) = case args of
            (t:i:_) | Just t' <- readMaybe t -> (t', T.pack i)
            -- default timeout: 3 minutes
            -- default input: null JSON value
            _                                -> (180, "null")

    -- HTTP Manager
    mgr <- newManager tlsManagerSettings

    -- AWS environment
    env' <- AWS.newEnvWith AWS.Discover Nothing mgr
    let env = env' & AWS.envRegion .~ AWS.Frankfurt

    withStderrLogger $ \lgr -> runLogT fnNameT lgr $ do
        logInfoI "Lambda $name started" $ object [ "name" .= fnNameT ]

        cfg <- getConfig (map toUpper fnName)
        case eitherDecodeStrict $ encodeUtf8 input of
            Left err -> do
                logAttention "Invalid JSON" err
            Right x  -> do
                req <- liftIO $ makeMockRequest () timeout
                res <- tryDeep' $ handler (getTimeRemaining req) env cfg lgr mgr x
                case res of
                    Left exc -> do
                        logAttention "Exception" $ show exc
                    Right x  -> do
                        logInfo "Success" x

                remaining <- liftIO (getTimeRemaining req)
                logInfoI "Lambda $name ending. Remaining $remaining ms" $
                    object [ "name" .= fnNameT, "remaining" .= remaining ]

awsLambda
    :: (FromJSON a, ToJSON b, NFData b, Configure cfg)
    => (LambdaContext -> AWS.Env -> cfg -> Logger -> Manager -> a -> LogT IO b)
    -> IO ()
awsLambda handler = defaultMain $ \req -> do
    let fnName = "LAMBDA"
    let fnNameT = fnName ^. packed

    -- HTTP Manager
    mgr <- newManager tlsManagerSettings

    -- AWS environment
    env' <- AWS.newEnvWith AWS.Discover Nothing mgr
    let env = env' & AWS.envRegion .~ AWS.Frankfurt

    output <- runLogT fnNameT lgr $ do
        logInfoI "Lambda $name started" $ object [ "name" .= fnNameT ]

        cfg <- getConfig (map toUpper fnName)
        res <- case eitherDecodeStrict (requestPayload req) of
            Left err -> do
                logAttention "Invalid JSON" err
                return $ FailureResponse "decode" (T.pack err)
            Right x  -> do
                res <- tryDeep' $ handler (getTimeRemaining req) env cfg lgr mgr x
                case res of
                    Left exc -> do
                        logAttention "Exception" $ show exc
                        return $ FailureResponse "decode" (T.pack $ displayException exc)
                    Right x  -> return $ jsonResponse x

        remaining <- liftIO (getTimeRemaining req)
        logInfoI "Lambda $name ending. Remaining $remaining ms" $
            object [ "name" .= fnNameT, "remaining" .= remaining ]

        return res

    -- Write aws env
    meters <- liftIO values
    unless (null meters) $ do
        let mkDatum (n, v) = AWS.metricDatum ("Count: " <> n)
                & AWS.mdValue      ?~ fromIntegral v
                & AWS.mdUnit       ?~ AWS.Count
                & AWS.mdDimensions .~ [AWS.dimension "FunctionName" fnNameT]
        let meterDatums = map mkDatum (Map.toList meters)

        AWS.runResourceT $ AWS.runAWS env $
            void $ AWS.send $ AWS.putMetricData "Lambda"
                    & AWS.pmdMetricData .~ meterDatums

    return output
  where
    lgr :: Logger
    lgr = Logger
        { loggerWriteMessage = \msg -> BS8.putStrLn (encodeUtf8 $ showLogMessage msg) >> hFlush stdout
        , loggerWaitForWrite = return ()
        , loggerShutdown     = hFlush stdout
        }

tryDeep' :: NFData a => LogT IO a -> LogT IO (Either SomeException a)
tryDeep' (LogT (ReaderT f)) = LogT $ ReaderT $ \r -> tryDeep (f r)

-- | Render a 'LogMessage' to 'Text'.
showLogMessage :: LogMessage -> T.Text
showLogMessage LogMessage{..} = T.concat $
    [ T.toUpper $ showLogLevel lmLevel
    , " "
    , T.intercalate "/" $ lmComponent : lmDomain
    , ": "
    , lmMessage
    ] ++
    if lmData == emptyObject
    then []
    else [" ", textifyData lmData]
  where
    textifyData :: Value -> T.Text
    textifyData = TE.decodeUtf8 . LBS.toStrict . encode
