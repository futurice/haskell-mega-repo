{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.Lambda (
    makeAwsLambda,
    AwsLambdaHandler,
    ) where

import Data.Aeson
       (FromJSON, ToJSON, Value, eitherDecodeStrict, encode, object, (.=))
import Data.Aeson.Types    (emptyObject)
import Foreign.C           (CString, withCString)
import Foreign.Ptr         (Ptr)
import Futurice.EnvConfig  (Configure, getConfig)
import Futurice.Prelude
import GHC.Stats           (getRTSStats)
import Log                 (LogMessage (..), LogT, runLogT, showLogLevel)
import Log.Internal.Logger (Logger (..))
import Log.Monad           (LogT (..))
import Prelude ()
import System.Environment  (getArgs)

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE

-- | A type for AWS Lambda Handler.
type AwsLambdaHandler = CString -> Ptr LambdaContext -> Ptr LoggingFunc -> IO CString

-- import some Python utilities
foreign import ccall "PyObject_CallFunction" logC :: Ptr LoggingFunc -> CString -> CString -> IO (Ptr PyObject)
foreign import ccall "Py_DecRef" pyDecRef :: Ptr PyObject -> IO ()

-- LambdaContext
-- https://docs.aws.amazon.com/lambda/latest/dg/python-context-object.html#python-context-object-methods
--
-- Currently unused.
data LambdaContext

-- | A special type for (callable) logging function.
data LoggingFunc

-- | Generic Python objects.
data PyObject

-- | Make an AWS Lambda handler.
makeAwsLambda
    :: (FromJSON a, ToJSON b, NFData b, Configure cfg)
    => (cfg -> Logger -> Manager -> a -> LogT IO b)
    -> AwsLambdaHandler
makeAwsLambda handler input _ lf = do
    inputBS  <- BS.packCString input
    mgr <- newManager tlsManagerSettings
    output <- runLogT "lambda" lgr $ do
        cfg <- getConfig "LAMBDA"
        case eitherDecodeStrict inputBS of
            Left err -> do
                logAttention "Invalid JSON" err
                encodeErr err
            Right x  -> do
                res <- tryDeep' $ handler cfg lgr mgr x
                case res of
                    Left exc -> do
                        logAttention "Exception" $ show exc
                        encodeErr $ show exc
                    Right x  -> return $ LBS.toStrict $ encode x
    BS.useAsCString output return
  where
    encodeErr err = return $ LBS.toStrict $ encode $ object
        [ "error" .= err
        ]

    logString :: String -> IO ()
    logString s =
        withCString "s" $ \fmt ->
        withCString s   $ \s' -> do
            res <- logC lf fmt s'
            -- PyObject_CallFunction returns new reference, so we dereference it
            -- Manual memory management: duh.
            pyDecRef res

    lgr :: Logger
    lgr = Logger
        { loggerWriteMessage = logString . T.unpack . showLogMessage
        , loggerWaitForWrite = return ()
        , loggerShutdown     = return ()
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
