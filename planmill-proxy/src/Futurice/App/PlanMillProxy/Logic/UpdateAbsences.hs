{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.PlanMillProxy.Logic.UpdateAbsences where

import Data.Aeson                   (FromJSON, object, (.=))
import Data.Binary.Tagged
       (HasSemanticVersion, HasStructuralInfo, taggedEncode)
import Data.Constraint
import Data.Time                    (addDays)
import Futurice.Postgres
import Futurice.Postgres.SqlBuilder
import Futurice.Prelude
import PlanMill.Worker              (Workers, submitPlanMill, withWorkers)
import Prelude ()

import Futurice.App.PlanMillProxy.Types (Ctx (..))

import qualified Database.PostgreSQL.Simple as Postgres
import qualified PlanMill                   as PM
import qualified PlanMill.Types.Query       as PM

-- Updating absences take more than 15 minutes, so we can't use Lambdas for those
-- For now try to update them in planmill-proxy. Later make better logic with only
-- updates the needed data
updateAbsences :: Manager -> Ctx -> IO ()
updateAbsences mgr ctx = runLogT "update-absences" (ctxLogger ctx) $ do
    -- Setup
    let pool = ctxPostgresPool ctx
    now <- currentTime

   -- We want a UTC 02:00 point before `now`.
    let UTCTime today offset = now
    let stamp
          | offset < 7200 = UTCTime (addDays (-1) today) 7200
          | otherwise     = UTCTime today 7200

    -- limit
    let limit = 1000

    withWorkers lgr mgr (ctxPlanmillCfg ctx) ["worker1", "worker2", "worker3"] $ \ws -> do
        logInfoI "Fetching outdated queries, limit $limit" $ object
            [ "older-than" .= stamp
            , "limit"      .= limit
            ]

        qs <- safePoolQueryM pool "planmillproxy" $  do
            tbl <- from_ "cache"
            fields_ tbl ["query"]
            where_ [ ecolumn_ tbl "updated", " < ", eparam_ stamp ]
            where_ [ ecolumn_ tbl "query", " similar to ", eparam_ ("%absence%" :: String)]
            orderby_ tbl "updated" ASC
            limit_ limit

        logInfo_ $ "Updating " <> textShow (length qs) <> " cache items"

        void $ runExceptT $ for_ qs $ \(Only (PM.SomeQuery q)) -> do
            when (isSlowQuery q) $ do
                res <- lift $ fetch ws pool q
                case res of
                  Right () -> pure ()
                  Left exc -> do
                      logAttention "Update failed" $ object
                          [ "query" .= q
                          , "exc"   .= show exc
                          ]
                      void $ safePoolExecute pool deleteQuery (Postgres.Only q)

  where
    fetch :: HasPostgresPool ctx => Workers -> ctx -> PM.Query a -> LogT IO (Either SomeException ())
    fetch ws ctx q = case (binaryDict, semVerDict, structDict, nfdataDict, fromJsonDict) of
        (Dict, Dict, Dict, Dict, Dict) -> fetch' ws ctx q
      where
        binaryDict   = PM.queryDict (Proxy :: Proxy Binary) q
        semVerDict   = PM.queryDict (Proxy :: Proxy HasSemanticVersion) q
        structDict   = PM.queryDict (Proxy :: Proxy HasStructuralInfo) q
        nfdataDict   = PM.queryDict (Proxy :: Proxy NFData) q
        fromJsonDict = PM.queryDict (Proxy :: Proxy FromJSON) q

    fetch'
        :: (Binary a, NFData a, FromJSON a, HasStructuralInfo a, HasSemanticVersion a, HasPostgresPool ctx)
        => Workers -> ctx -> PM.Query a
        -> LogT IO (Either SomeException ())
    fetch' ws ctx q = do
        x <- liftIO $ tryDeep $ submitPlanMill ws $ PM.queryToRequest q
        case x of
            Left err -> return (Left err)
            Right x' -> do
                storeInPostgres ctx q x'
                return (Right ())

    isSlowQuery (PM.QueryPagedGet PM.QueryTagAbsence _ _) = True
    isSlowQuery (PM.QueryPagedGet PM.QueryTagAccount _ _) = True
    isSlowQuery _                                         = False

    lgr = ctxLogger ctx

deleteQuery :: Postgres.Query
deleteQuery = "DELETE FROM planmillproxy.cache WHERE query = ?;"

storeInPostgres
    :: (Binary a, HasSemanticVersion a, HasStructuralInfo a, HasPostgresPool ctx)
    => ctx -> PM.Query a -> a -> LogT IO ()
storeInPostgres ctx q x = do
    -- -- logInfo_ $ "Storing in postgres" <> textShow q
    i <- safePoolExecute ctx postgresQuery (q, Postgres.Binary $ taggedEncode x)
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
