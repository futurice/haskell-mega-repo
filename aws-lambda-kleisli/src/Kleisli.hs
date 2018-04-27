module Kleisli where

import           Control.Lens         (deep, (%~), (&))
import           Data.Aeson           (Value, eitherDecodeStrict, encode)
import           Data.Aeson.Lens      (_String)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           Foreign.C            (CString, withCString)
import           Foreign.Ptr          (Ptr)
import           GHC.Stats            (getRTSStats)
import           System.Environment   (getArgs)

-- export handler
foreign export ccall kleisliHaskellHandler :: CString -> Ptr LambdaContext -> Ptr LoggingFunc -> IO CString

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

-- | An exported handler. Long name to avoid name clashes.
kleisliHaskellHandler :: CString -> Ptr LambdaContext -> Ptr LoggingFunc -> IO CString
kleisliHaskellHandler input _ lf = do
    inputBS  <- BS.packCString input
    output <- handlerBS logString inputBS
    BS.useAsCString output return
  where
    logString :: String -> IO ()
    logString s =
        withCString "s" $ \fmt ->
        withCString s   $ \s' -> do
            res <- logC lf fmt s'
            -- PyObject_CallFunction returns new reference, so we dereference it
            -- Manual memory management: duh.
            pyDecRef res

-- | Handler working on 'ByteString's.
handlerBS :: (String -> IO ()) -> BS.ByteString -> IO BS.ByteString
handlerBS logString input = case eitherDecodeStrict input of
    Left err -> return $ TE.encodeUtf8 $ T.pack err
    Right x  -> fmap (LBS.toStrict . encode) $ handler logString x

-- | Handler working on values. Note: you could use anything which has
-- 'FromJSON' and 'ToJSON' instances.
handler :: (String -> IO ()) -> Value -> IO Value
handler logString x = do
    logString "Logging from Haskell"
    logString . show =<< getArgs
    logString . show =<< getRTSStats
    return $ x
        & deep _String %~ T.toUpper
