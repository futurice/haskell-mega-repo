{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.FUM.MachineAPI (
    FUMMachineAPI,
    fumMachineApi,
    -- * Class
    MonadFUM6 (..),
    -- * haxl
    FUM6 (..),
    SomeFUM6 (..),
    SomeFUM6Response (..),
    fumHaxlRequest,
    initDataSource,
    ) where

import Control.Concurrent.Async (async, waitCatch)
import Data.Aeson.Compat
       (FromJSON (..), ToJSON (..), decode, encode, object, withObject, (.:),
       (.=))
import Data.Aeson.Types         (Object, Parser)
import Data.GADT.Compare        (GEq (..))
import Data.Type.Equality
import FUM.Types.GroupName      (GroupName)
import FUM.Types.Login          (Login)
import Futurice.Prelude
import Haxl.Core
import Prelude ()
import Servant.API

import qualified Data.Swagger        as S
import qualified Network.HTTP.Client as HTTP

-------------------------------------------------------------------------------
-- Servant API
-------------------------------------------------------------------------------

type FUMMachineAPI = "haxl" :> Summary "Haxl endpoint" :> ReqBody '[JSON] [SomeFUM6] :> Post '[JSON] [SomeFUM6Response]
    :<|> "groups" :> Summary "Get group members" :> Capture "group-name" GroupName :> "employees" :> Get '[JSON] (Set Login)

fumMachineApi :: Proxy FUMMachineAPI
fumMachineApi = Proxy

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

class Monad m => MonadFUM6 m where
    fum6 :: FUM6 a -> m a

-------------------------------------------------------------------------------
-- Haxl
-------------------------------------------------------------------------------

data FUM6 a where
    FUMGroupEmployees :: GroupName -> FUM6 (Set Login)

deriving instance Eq (FUM6 a)
deriving instance Ord (FUM6 a)
deriving instance Show (FUM6 a)

instance Hashable (FUM6 a) where
    hashWithSalt salt (FUMGroupEmployees n) = salt `hashWithSalt` (0 :: Int)
        `hashWithSalt` n

instance ShowP FUM6 where
    showp = show

instance StateKey FUM6 where
    data State FUM6 = FUM6State !Logger !Manager !HTTP.Request

instance DataSourceName FUM6 where
    dataSourceName _ = "FUM6.Request"

instance GEq FUM6 where
    geq (FUMGroupEmployees n) (FUMGroupEmployees n')
        | n == n' = Just Refl
    geq _ _ = Nothing

-------------------------------------------------------------------------------
-- Haxl Request
-------------------------------------------------------------------------------

-- | Some Request
data SomeFUM6 where
    SomeFUM6 :: FUM6 a -> SomeFUM6

instance ToJSON SomeFUM6 where
    toJSON = object . someFUM6ToJSON

someFUM6ToJSON :: SomeFUM6 -> [AesonPair]
someFUM6ToJSON (SomeFUM6 (FUMGroupEmployees n)) =
    [ "tag"       .=  ("group-employees" :: Text)
    , "groupName" .= n
    ]

instance FromJSON SomeFUM6 where
    parseJSON = withObject "SomeFUM6" parseSomeFUM6

parseSomeFUM6 :: Object -> Parser SomeFUM6
parseSomeFUM6 obj = do
    tag <- obj .: "tag"
    case (tag :: Text) of
        "group-employees" -> do
            n <- obj .: "groupName"
            pure (SomeFUM6 (FUMGroupEmployees n))
        _ -> fail $ "Invalid request tag: " ++ show tag

instance S.ToSchema SomeFUM6 where
    declareNamedSchema _ = pure $ S.NamedSchema (Just "FUM6 Request") mempty

-------------------------------------------------------------------------------
-- Haxl Response
-------------------------------------------------------------------------------

data SomeFUM6Response where
    SomeFUM6Response :: FUM6 a -> a -> SomeFUM6Response

instance ToJSON SomeFUM6Response where
    toJSON (SomeFUM6Response req res) = object
        $ p
        : someFUM6ToJSON (SomeFUM6 req)
      where
        p :: AesonPair
        p = case req of
            FUMGroupEmployees _ -> "response" .= res

instance FromJSON SomeFUM6Response where
    parseJSON = withObject "SomeFUM6Response" $ \obj -> do
        SomeFUM6 req <- parseSomeFUM6 obj
        case req of
            FUMGroupEmployees _ -> SomeFUM6Response req <$> obj .: "response"

instance S.ToSchema SomeFUM6Response where
    declareNamedSchema _ = pure $ S.NamedSchema (Just "FUM6 Response") mempty

-------------------------------------------------------------------------------
-- Haxl DataSource
-------------------------------------------------------------------------------

fumHaxlRequest :: FUM6 a -> GenHaxl u a
fumHaxlRequest req = case req of
    FUMGroupEmployees _ -> dataFetch req

initDataSource :: Logger -> Manager -> HTTP.Request -> State FUM6
initDataSource = FUM6State

instance DataSource u FUM6 where
    fetch (FUM6State lgr mgr baseReq) _flags _userEnv bf =
        AsyncFetch $ \inn -> runLogT "fum6-haxl" lgr $ do
            let queries = map toQuery bf
            logTrace "request" queries
            a <- liftIO $ async $ do
                res <- HTTP.httpLbs (mkRequest queries) mgr
                decode (HTTP.responseBody res)

            -- allow inner block to perform
            liftIO inn

            -- wait
            res <- liftIO $ waitCatch a

            -- Put results
            case res of
                Left exc -> for_ bf $ \(BlockedFetch _ v) ->
                    liftIO $ putFailure' v exc
                Right vs -> putResults bf vs
      where
        toQuery (BlockedFetch req _) = SomeFUM6 req

        mkRequest reqs = baseReq
            { HTTP.requestBody = HTTP.RequestBodyLBS (encode reqs)
            , HTTP.method = "POST"
            , HTTP.requestHeaders
                = ("Content-Type", "application/json")
                : ("Accept", "application/json")
                : HTTP.requestHeaders baseReq
            }

        putResults :: [BlockedFetch FUM6] -> [SomeFUM6Response] -> LogT IO ()
        -- if no more blocked fetches, we are done
        putResults [] ress =
            unless (null ress) $ logAttention_ "Extra responses"

        -- if we have blocked fetch, but no result:
        -- report error
        putResults (BlockedFetch _q v : _)  [] =
            putFailure' v (FUM6BatchError "Truncated response")
{-
        -- backend might return error as well
        putResults (BlockedFetch _q v : rest) (Left err : bss) = do
            putFailure' v (FUM6BatchError err)
            putResults rest bss
-}
        -- on success, try decode
        putResults (BlockedFetch req v : reqs) (SomeFUM6Response req' x : ress) = do
            liftIO $ putResult v (coerceResponse req req' x)
            putResults reqs ress

        coerceResponse :: FUM6 a -> FUM6 b -> b -> Either SomeException a
        coerceResponse a b x = case geq a b of
            Just Refl -> pure x
            Nothing   -> throwM (FUM6BatchError "Unmatching request-response")

putFailure' :: (MonadIO m, Exception e) => ResultVar a -> e -> m ()
putFailure' v = liftIO . putFailure v . SomeException

-- | FUM6 batch error
newtype FUM6BatchError = FUM6BatchError Text
    deriving (Show, Typeable)

instance Exception FUM6BatchError
