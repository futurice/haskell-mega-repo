{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.PlanMillProxy.Logic.UpdateAbsences where

import Data.Aeson                   (FromJSON, object, (.=))
import Data.Binary.Tagged           (Structured, structuredEncode)
import Data.Constraint
import Data.Time                    (addDays)
import Futurice.Postgres
import Futurice.Postgres.SqlBuilder
import Futurice.Prelude
import PlanMill.Worker              (submitPlanMill)
import Prelude ()

import Futurice.App.PlanMillProxy.Types (Ctx (..))

import qualified Database.PostgreSQL.Simple as Postgres
import qualified PlanMill.Types.Query       as PM

-- Updating absences take more than 15 minutes, so we can't use Lambdas for those
-- For now try to update them in planmill-proxy. Later make better logic with only
-- updates the needed data
updateAbsences ::  Ctx -> IO ()
updateAbsences ctx = runLogT "update-absences" (ctxLogger ctx) $ do
    -- Setup
    now <- currentTime

   -- We want a UTC 02:00 point before `now`.
    let UTCTime today offset = now
    let stamp
          | offset < 7200 = UTCTime (addDays (-1) today) 7200
          | otherwise     = UTCTime today 7200

    -- limit
    let limit = 1000

    logInfoI "Fetching outdated queries, limit $limit" $ object
        [ "older-than" .= stamp
        , "limit"      .= limit
        ]

    qs <- safePoolQueryM ctx "planmillproxy" $  do
        tbl <- from_ "cache"
        fields_ tbl ["query"]
        where_ [ ecolumn_ tbl "updated", " < ", eparam_ stamp ]
        where_ [ ecolumn_ tbl "query", " similar to ", eparam_ ("%absence%" :: String)]
        orderby_ tbl "updated" ASC
        limit_ limit

    logInfo_ $ "Updating " <> textShow (length qs) <> " cache items"

    void $ runExceptT $ for_ qs $ \(Only (PM.SomeQuery q)) -> do
        when (isSlowQuery q) $ do
            res <- lift $ fetch q
            case res of
              Right () -> pure ()
              Left exc -> do
                  logAttention "Update failed" $ object
                      [ "query" .= q
                      , "exc"   .= show exc
                      ]
                  void $ safePoolExecute ctx deleteQuery (Postgres.Only q)

  where
    fetch :: PM.Query a -> LogT IO (Either SomeException ())
    fetch q = case (binaryDict, structDict, nfdataDict, fromJsonDict) of
        (Dict, Dict, Dict, Dict) -> fetch' q
      where
        binaryDict   = PM.queryDict (Proxy :: Proxy Binary) q
        structDict   = PM.queryDict (Proxy :: Proxy Structured) q
        nfdataDict   = PM.queryDict (Proxy :: Proxy NFData) q
        fromJsonDict = PM.queryDict (Proxy :: Proxy FromJSON) q

    fetch'
        :: (Binary a, NFData a, FromJSON a, Structured a)
        => PM.Query a
        -> LogT IO (Either SomeException ())
    fetch' q = do
        x <- liftIO $ tryDeep $ submitPlanMill ws $ PM.queryToRequest q
        case x of
            Left err -> return (Left err)
            Right x' -> do
                storeInPostgres ctx q x'
                return (Right ())

    isSlowQuery (PM.QueryPagedGet PM.QueryTagAbsence _ _) = True
    isSlowQuery (PM.QueryPagedGet PM.QueryTagAccount _ _) = True
    isSlowQuery _                                         = False

    ws = ctxWorkers ctx

deleteQuery :: Postgres.Query
deleteQuery = "DELETE FROM planmillproxy.cache WHERE query = ?;"

storeInPostgres
    :: (Binary a, Structured a, HasPostgresPool ctx)
    => ctx -> PM.Query a -> a -> LogT IO ()
storeInPostgres ctx q x = do
    -- -- logInfo_ $ "Storing in postgres" <> textShow q
    i <- safePoolExecute ctx postgresQuery (q, Postgres.Binary $ structuredEncode x)
    when (i == 0) $
        logAttention_ $ "Storing in postgres failed: " <> textShow q
  where
    postgresQuery = fromString $ unwords
        [ "INSERT INTO planmillproxy.cache as c (query, data)"
        , "VALUES (?, ?)"
        , "ON CONFLICT (query) DO UPDATE"
        , "SET data = EXCLUDED.data,"
        , "    viewed = case WHEN c.viewed <= 0 THEN c.viewed-1 ELSE 0 END,"
        , "    updated = now(),"
        , "    variance = random()"
        , "WHERE c.query = EXCLUDED.query"
        , ";"
        ]
