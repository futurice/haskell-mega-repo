{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.PlanMillProxy.Logic.Timereports (
    selectTimereports,
    ) where

import Data.Binary.Tagged        (structuredDecode)
import Futurice.Prelude
import Numeric.Interval.NonEmpty (inf, sup)
import Prelude ()

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Vector                as V
import qualified Database.PostgreSQL.Simple as Postgres
import qualified PlanMill                   as PM

import Futurice.App.PlanMillProxy.Logic.Common

-- | Select timereports
--
-- If data in cache is invalid, we prune it, and return zero timereports.
selectTimereports
    :: HasPostgresPool ctx => ctx
    -> PM.UserId -> Maybe (PM.Interval Day) -> LIO PM.Timereports
selectTimereports ctx uid minterval = do
    res <- case minterval of
        Nothing       -> safePoolQuery ctx selectQueryWithoutInterval (Postgres.Only uid)
        Just interval -> safePoolQuery ctx selectQueryWithInterval (uid, inf interval, sup interval)
    res' <- liftIO $ tryDeep $ return $ V.fromList $ map selectTransform res
    case res' of
        Right x -> return x
        Left exc -> do
            logAttention_ $ "selectTimereports: " <> textShow exc
            _ <- case minterval of
                Nothing       -> safePoolExecute ctx deleteQueryWithoutInterval (Postgres.Only uid)
                Just interval -> safePoolExecute ctx deleteQueryWithInterval (uid, inf interval, sup interval)
            return mempty
  where
    selectTransform
        :: Postgres.Only (Postgres.Binary BSL.ByteString)
        -> PM.Timereport
    selectTransform (Postgres.Only (Postgres.Binary bs)) = structuredDecode bs

    selectQueryWithInterval :: Postgres.Query
    selectQueryWithInterval = fromString $ unwords $
        [ "SELECT (data) FROM planmillproxy.timereports"
        , "WHERE uid = ? AND day >= ? AND day <= ?"
        , ";"
        ]

    selectQueryWithoutInterval :: Postgres.Query
    selectQueryWithoutInterval = fromString $ unwords $
        [ "SELECT (data) FROM planmillproxy.timereports"
        , "WHERE uid = ?"
        , ";"
        ]

    deleteQueryWithoutInterval :: Postgres.Query
    deleteQueryWithoutInterval = fromString $ unwords $
        [ "DELETE FROM planmillproxy.timereports"
        , "WHERE uid = ?"
        , ";"
        ]

    deleteQueryWithInterval :: Postgres.Query
    deleteQueryWithInterval = fromString $ unwords $
        [ "DELETE FROM planmillproxy.timereports"
        , "WHERE uid = ? AND day >= ? AND day <= ?"
        , ";"
        ]
