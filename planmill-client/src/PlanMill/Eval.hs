{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module PlanMill.Eval (evalPlanMill) where

import PlanMill.Internal.Prelude

import Control.Monad.Http         (MonadHttp (..), httpLbs)
import Data.Aeson.Compat          (eitherDecode)
import Data.IORef                 (IORef, atomicModifyIORef', newIORef)
import Futurice.Metrics.RateMeter (mark)
import Network.HTTP.Client
       (Request, RequestBody (..), method, parseRequest, path, queryString,
       requestBody, requestHeaders, responseBody, responseStatus,
       responseTimeout, responseTimeoutMicro, setQueryString)
import Network.HTTP.Types         (Header, Status (..), statusIsSuccessful)
import System.IO.Unsafe           (unsafePerformIO)
import Text.Printf                (printf)

-- Qualified imports
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8  as BS8
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Map               as Map
import qualified Data.Text              as T
import qualified Data.Vector            as V

-- PlanMill import
import PlanMill.Auth    (Auth (..), getAuth)
import PlanMill.Classes
import PlanMill.Types

-------------------------------------------------------------------------------
-- Counter
-------------------------------------------------------------------------------

counter :: IORef Int
counter = unsafePerformIO $ newIORef 0
{-# NOINLINE counter #-}

-------------------------------------------------------------------------------
-- Evaluate
-------------------------------------------------------------------------------

evalPlanMill
    :: forall m env e a.
        ( MonadHttp m, MonadThrow m, MonadLog m -- MonadTime m implied by MonadLog
        , MonadReader env m, HasPlanMillCfg env
        , MonadCRandom e m, ContainsCryptoGenError e
        , MonadIO m  -- counter, mark
        , MonadClock m -- clocked
        , FromJSON a
        )
    => PlanMill a -> m a
evalPlanMill pm = do
    baseReq <- mkBaseReq pm
    case pm of
        PlanMillGet qs _ ->
            singleReq baseReq qs Nothing
        PlanMillPagedGet qs _ ->
            pagedReq baseReq qs
        PlanMillPost e body _ -> do
            let req = addHeader ("Content-Type", "application/json;charset=UTF-8") $
                    baseReq { method = "POST", requestBody = RequestBodyLBS body }
            singleReq req mempty $ case e of
                Nothing   -> Nothing
                Just Refl -> pure ()
        PlanMillDelete _ -> do
            let req = baseReq { method = "DELETE" }
            singleReq req mempty (pure ())
  where
    mkBaseReq :: forall b. PlanMill b -> m Request
    mkBaseReq planmill = do
        baseUrl <- view planmillCfgBaseUrl
        parseRequest $ baseUrl <> fromUrlParts (requestUrlParts planmill)

    singleReq
        :: forall b. FromJSON b
        => Request
        -> QueryString
        -> Maybe b  -- ^ Possibly return when empty response
        -> m b
    singleReq req qs d = do
        let req' = setQueryString' qs req
        let m = BS8.unpack (method req')
        let url = BS8.unpack (path req') <> BS8.unpack (queryString req')
        uniqId <- liftIO $ textShow <$> atomicModifyIORef' counter (\c -> (succ c, c))
        logLocalDomain ("req-" <> uniqId) $ do
            logTrace_ $ T.pack $ "req " <> m <> " " <> url
            -- We need to generate auth (nonce) at each req
            auth <- getAuth
            let req'' = (addHeader (authHeader auth) req')
            -- BEGIN HACK: 2018-12-17, PlanMill is sad so we do this
                      { responseTimeout =
                          if BS8.isSuffixOf "tasks/28532" (path req)
                            || BS8.isSuffixOf "absences" (path req)
                          then responseTimeoutMicro $ 120 * 1000000
                          else responseTimeout req' -- otherwise do nothing
                      }
            -- END HACK
            (dur, res) <- clocked $ httpLbs req''
            let status@Status {..} = responseStatus res
            let body = responseBody res
            let dur' = timeSpecToSecondsD dur
            let dur'' = printf "%.06fs" dur'
            logTrace_ $ "res " <> textShow statusCode <> " " <> textShow statusMessage <> "; took " <> dur'' ^. packed
            liftIO $ mark "PlanMill request"
            if isn't _Empty body
                then
                    -- logTrace_ $ "response body: " <> decodeUtf8Lenient (responseBody res ^. strict)
                    if statusIsSuccessful status
                        then parseResult url body
                        else do
                            -- if we have "default" response, we return it in case of error
                            let exc = parseError url body
                            case d of
                                Nothing -> throwM exc
                                Just d' -> do
                                    logAttention "Recoverable error" $ object [ "exc" .= textShow exc ]
                                    return d'
                else do
                    logTrace_ "empty response"
                    case d of
                        Just d' -> pure d'
                        Nothing -> throwM (emptyError url)

    setQueryString' :: QueryString -> Request -> Request
    setQueryString' qs = setQueryString (f <$> Map.toList qs)
      where
        f (a, b) = (encodeUtf8 a, Just $ encodeUtf8 b)

    parseResult :: forall b .(FromJSON b) => String -> LBS.ByteString -> m b
    parseResult url body =
        case eitherDecode body of
            Right x  -> pure x
            Left err -> throwM $ DecodeError url err

    parseError :: String -> LBS.ByteString -> PlanMillError
    parseError url body =
        case eitherDecode body of
            Right exc -> ErrorResponse url exc
            Left err  -> DecodeError url err

    emptyError url = DecodeError url "empty input"

    -- See https://online.planmill.com/pmtrial/schemas/v1_5/index.html#projects_get
    -- for explanation of paged response query string parameters
    --
    -- We actually need the type equality constraint
    -- to use vector's length
    pagedReq
        :: forall b b'. (b ~ V.Vector b', FromJSON b)
        => Request -> QueryString
        -> m b
    pagedReq req qs = go mempty
      where
        go :: V.Vector b' -> m b
        go acc = do
            -- We are for one too much, because if `nextrows` is over amount
            -- the collection from beginning is returned
            let qs' = Map.fromList
                    [ ("rowcount", T.pack $ show $ rowCount + 1)
                    , ("nextrows", T.pack $ show $ V.length acc + 1)
                    ]
            res <- singleReq req (qs <> qs') (pure V.empty)
            if V.length res <= rowCount
                then pure (acc <> res)
                else go (acc <> V.take rowCount res)

        -- The PlanMill documentation doesn't specify the maximum rows we
        -- can ask for, so we empirically found this limit works
        rowCount :: Int
        rowCount = 1000

authHeader :: Auth -> Header
authHeader (Auth (Ident uid) (Nonce nonce) ts sig) = ("x-PlanMill-Auth",
    "user:"      <> bsShow uid                   <> ";" <>
    "nonce:"     <> nonce                        <> ";" <>
    "timestamp:" <> bsShow (utcTimeToInteger ts) <> ";" <>
    "signature:" <> Base64.encode sig)

addHeader :: Header -> Request -> Request
addHeader header req = req
    { requestHeaders = header : requestHeaders req
    }
