{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FlowdockProxy.DB where

import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Futurice.Generics
import Futurice.Generics.SOP (sopParseJSON, sopToEncoding, sopToJSON)
import Futurice.Prelude
import Prelude ()

#ifdef MIN_VERSION_ghc_compact
import qualified GHC.Compact as C
#else
import Control.DeepSeq (force)
#endif

import qualified Chat.Flowdock.REST                   as FD
import qualified Data.Text                            as T
import qualified Data.Text.Short                      as TS
import qualified Data.TextSet.Unboxed                 as TextSet
import qualified Database.PostgreSQL.Simple.FromField as PQ
import qualified Database.PostgreSQL.Simple.FromRow   as PQ
import qualified Database.PostgreSQL.Simple.ToField   as PQ
import qualified Database.PostgreSQL.Simple.ToRow     as PQ
import qualified Database.PostgreSQL.Simple.Types     as PQ
import qualified Futurice.Postgres                    as PQ

-------------------------------------------------------------------------------
-- EpochTime'
-------------------------------------------------------------------------------

newtype EpochTime' = EpochTime' { getEpochTime' :: Int64 }
  deriving newtype (Eq, Ord,  Show, NFData)

epochTimeToUtcTime :: EpochTime' -> UTCTime
epochTimeToUtcTime = posixSecondsToUTCTime . realToFrac . getEpochTime'

utcTimeToEpochTime :: UTCTime -> EpochTime'
utcTimeToEpochTime = EpochTime' . truncate . utcTimeToPOSIXSeconds

instance ToJSON EpochTime' where
    toJSON     = toJSON     . epochTimeToUtcTime
    toEncoding = toEncoding . epochTimeToUtcTime

instance FromJSON EpochTime' where
    parseJSON = fmap utcTimeToEpochTime . parseJSON

instance ToSchema EpochTime' where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy UTCTime)

instance PQ.ToField EpochTime' where
    toField = PQ.toField . epochTimeToUtcTime

instance PQ.FromField EpochTime' where
    fromField x y = fmap utcTimeToEpochTime (PQ.fromField x y)

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data Row = Row
    { rowMessageId :: !FD.MessageId
    , rowUser      :: !FD.UserId
    , rowCreatedAt :: !EpochTime'
    , rowTags      :: !TextSet.TextSet
    , rowText      :: !ShortText
    }
  deriving stock (Eq, Ord, Show, GhcGeneric)
  deriving anyclass (NFData, SopGeneric, HasDatatypeInfo)

instance ToJSON Row where
    toJSON     = sopToJSON
    toEncoding = sopToEncoding

instance FromJSON Row where
    parseJSON = sopParseJSON

instance ToSchema Row where declareNamedSchema = sopDeclareNamedSchema

instance PQ.FromRow Row where
    fromRow = Row
        <$> (FD.mkIdentifier . fromInteger <$> PQ.field)
        <*> (FD.mkIdentifier . fromInteger <$> PQ.field)
        <*> PQ.field
        <*> (TextSet.fromList . map TS.fromText . PQ.fromPGArray <$> PQ.field)
        <*> (TS.fromText <$> PQ.field)

instance PQ.ToRow Row where
    toRow (Row mid uid created tags text) =
        [ PQ.toField $ toInteger $ FD.getIdentifier mid
        , PQ.toField $ toInteger $ FD.getIdentifier uid
        , PQ.toField created
        , PQ.toField $ PQ.PGArray $ map TS.toText $ TextSet.toList tags
        , PQ.toField (TS.toText text)
        ]

messageToRow :: FD.Message -> Maybe Row
messageToRow msg = Row
    (msg ^. FD.msgId)
    (msg ^. FD.msgUser)
    (utcTimeToEpochTime $ msg ^. FD.msgCreatedAt)
    (filterTags $ msg ^. FD.msgTags)
    <$> messageContentToRow (msg ^. FD.msgContent)
  where
    messageContentToRow (FD.MTMessage text) = Just (TS.fromText text)
    messageContentToRow (FD.MTComment comm) = Just (TS.fromText $ comm ^. FD.commentText)
    messageContentToRow _                   = Nothing

    filterTags = fromList . mapMaybe (tagPredicate . FD.getTag) . toList
    tagPredicate t
          | T.isPrefixOf ":" t || T.isPrefixOf "influx:" t = Nothing
          | otherwise                                      = Just (TS.fromText t)

    -- "singletonises"  empty lists
    fromList [] = TextSet.empty
    fromList xs = TextSet.fromList xs

-------------------------------------------------------------------------------
-- Query
-------------------------------------------------------------------------------

queryRows :: PQ.HasPostgresPool ctx => ctx -> Int -> LogT IO [Row]
queryRows ctx flowId = do
    rows' <- PQ.safePoolQuery ctx query (PQ.Only flowId)
#ifdef MIN_VERSION_ghc_compact
    rowsCompact <- liftIO $ C.compact (reverse rows')
    let rows = C.getCompact rowsCompact
#else
    rows <- liftIO $ evaluate $ force $ reverse rows'
#endif
    return rows
  where
    query :: PQ.Query
    query = "SELECT message_id, user_id, created, tags, content FROM \"flowdock-proxy\".messages WHERE flow_id = ? ORDER by message_id ASC;"

-------------------------------------------------------------------------------
-- Insert
-------------------------------------------------------------------------------

insertRows :: PQ.HasPostgresPool ctx => ctx -> Int -> [Row] -> LogT IO ()
insertRows ctx flowId rows
    | null rows = return ()
    | otherwise =
    void $ PQ.safePoolExecuteMany ctx
        "INSERT INTO \"flowdock-proxy\".messages (flow_id, message_id, user_id, created, tags, content) VALUES (?, ?, ?, ?, ?, ?) ON CONFLICT (flow_id, message_id) DO UPDATE SET created = EXCLUDED.created, tags = EXCLUDED.tags, content = EXCLUDED.content;"
        [ (PQ.Only flowId PQ.:. row) | row <- rows ]
