{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FlowdockProxy.DB where

import Futurice.Prelude
import Prelude ()

#ifdef MIN_VERSION_ghc_compact
import qualified GHC.Compact as C
#else
import Control.DeepSeq (force)
#endif

import qualified Chat.Flowdock.REST                 as FD
import qualified Data.Text                          as T
import qualified Data.Vector                        as V
import qualified Database.PostgreSQL.Simple.FromRow as PQ
import qualified Database.PostgreSQL.Simple.ToField as PQ
import qualified Database.PostgreSQL.Simple.ToRow   as PQ
import qualified Database.PostgreSQL.Simple.Types   as PQ
import qualified Futurice.Postgres                  as PQ

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data Row = Row
    { rowMessageId :: !FD.MessageId
    , rowUser      :: !FD.UserId
    , rowCreatedAt :: !UTCTime
    , rowTags      :: ![FD.Tag]
    , rowText      :: !Text
    }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

instance PQ.FromRow Row where
    fromRow = Row
        <$> (FD.mkIdentifier . fromInteger <$> PQ.field)
        <*> (FD.mkIdentifier . fromInteger <$> PQ.field)
        <*> PQ.field
        <*> (map FD.Tag . PQ.fromPGArray <$> PQ.field)
        <*> PQ.field

instance PQ.ToRow Row where
    toRow (Row mid uid created tags text) =
        [ PQ.toField $ toInteger $ FD.getIdentifier mid
        , PQ.toField $ toInteger $ FD.getIdentifier uid
        , PQ.toField created
        , PQ.toField $ PQ.PGArray $ map FD.getTag tags
        , PQ.toField text
        ]

messageToRow :: FD.Message -> Maybe Row
messageToRow msg = Row
    (msg ^. FD.msgId)
    (msg ^. FD.msgUser)
    (msg ^. FD.msgCreatedAt)
    (filterTags $ msg ^. FD.msgTags)
    <$> messageContentToRow (msg ^. FD.msgContent)
  where
    messageContentToRow (FD.MTMessage text) = Just text
    messageContentToRow (FD.MTComment comm) = Just (comm ^. FD.commentText)
    messageContentToRow _                   = Nothing

    filterTags = toList . V.filter (not . tagPredicate . FD.getTag)
    tagPredicate t = T.isPrefixOf ":" t || T.isPrefixOf "influx:" t

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
        "INSERT INTO \"flowdock-proxy\".messages (flow_id, message_id, user_id, created, tags, content) VALUES (?, ?, ?, ?, ?, ?);"
        [ (PQ.Only flowId PQ.:. row) | row <- rows ]
