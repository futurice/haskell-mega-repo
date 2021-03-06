{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.PersonioProxy (defaultMain) where

import Control.Concurrent.Async (async)
import Control.Concurrent.STM
       (atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import Data.Aeson.Types         (object, parseEither, parseJSON, toJSON, (.=))
import FUM.Types.Login
import Futurice.IdMap           (IdMap, keysSet)
import Futurice.Integrations
import Futurice.Periocron
import Futurice.Prelude
import Futurice.Servant
import Futurice.Time
import Prelude ()
import Servant

import Futurice.App.PersonioProxy.API
import Futurice.App.PersonioProxy.Config
import Futurice.App.PersonioProxy.IndexPage
import Futurice.App.PersonioProxy.Logic
import Futurice.App.PersonioProxy.StatsPage
import Futurice.App.PersonioProxy.Types

import qualified Data.Map.Strict   as Map
import qualified Data.Set          as Set
import qualified Futurice.IdMap    as IdMap
import qualified Futurice.Postgres as Postgres
import qualified Personio          as P

#ifdef MIN_VERSION_ghc_compact
import GHC.Compact
       (compactAddWithSharing, compactSize, compactWithSharing, getCompact)
#else
import Control.DeepSeq (force)
#endif

server :: Ctx -> Server PersonioProxyAPI
server ctx = indexPage'
    :<|> statsPage'
    :<|> attritionRate ctx
    :<|> averageTargetMonthlyCompensation ctx
    :<|> personioRequest ctx
--    :<|> rawEmployees ctx
    :<|> employeePicture ctx
    :<|> employeeUsername ctx
    :<|> scheduleEmployees ctx
    :<|> inventoryEmployees ctx
    :<|> employeesChart ctx
    :<|> tribeEmployeesChart ctx
    :<|> careerLevelsChart ctx
    :<|> rolesDistributionChart ctx
  where
    indexPage' = liftIO $ do
        ps <- readTVarIO (ctxPersonioData ctx)
        return (indexPage $ IdMap.fromFoldable $ P.paEmployees ps)
    statsPage' = liftIO $ do
        ps <- readTVarIO (ctxPersonioData ctx)
        return (statsPage $ P.paEmployees ps)

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService       .~ PersonioProxyService
    & serverDescription   .~ "Make faster queries to Personio"
    & serverColour        .~ (Proxy :: Proxy ('FutuAccent 'AF6 'AC1))
    & serverApp personioProxyApi .~ server
    & serverEnvPfx        .~ "PERSONIOPROXY"

newCtx
    :: Logger
    -> Cache
    -> Manager
    -> P.Cfg
    -> Postgres.Pool Postgres.Connection
    -> P.PersonioAllData
    -> IO Ctx
newCtx lgr cache mgr cfg pool allData = do
    Ctx lgr cache mgr cfg pool
        <$> newTVarIO allData

selectLastQuery :: Postgres.Query
selectLastQuery = fromString $ unwords
    [ "SELECT contents FROM \"personio-proxy\".log"
    , "ORDER BY timestamp DESC LIMIT 1"
    , ";"
    ]

insertQuery :: Postgres.Query
insertQuery = fromString $ unwords
    [ "INSERT INTO \"personio-proxy\".log (contents) VALUES (?)"
    , ";"
    ]

makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
makeCtx (Config cfg pgCfg intervalMin) lgr mgr cache mq = do
    pool <- Postgres.createPostgresPool pgCfg
    -- employees
    employees <- runLogT "startup" lgr $ do
        rows <- Postgres.safePoolQuery_ pool selectLastQuery
        case rows of
            [] -> do
                logInfo_ "No previous Personio data in cache"
                return []

            -- Note: if Personio.Employee changes, we have to make adoptions here!
            (Only v : _) -> case parseEither parseJSON v of
                Right employees -> return employees
                Left err        -> do
                    logAttention "Cannot parse previous Personio data" err
                    return []

    -- context
    let emptyData = P.PersonioAllData employees [] mempty mempty
    ctx <- newCtx lgr cache mgr cfg pool emptyData

    -- jobs
    let fetchEmployees = P.evalPersonioReqIO mgr lgr cfg P.PersonioAll
    let intervalSec = unNDT (ndtConvert' intervalMin :: NDT 'Seconds NominalDiffTime) -- TODO: add this to futurice-prelude
    let employeesJob   = mkJob "Update personio data" (updateJob ctx fetchEmployees) $ every intervalSec

    pure (ctx, [ employeesJob ])
  where
    updateJob :: Ctx -> IO P.PersonioAllData -> IO ()
    updateJob ctx fetchEmployees = do
        !(P.PersonioAllData employees' validations' cl clRole) <- fetchEmployees
        let employees = filter notMachine employees'
        let validations = filter (notMachine . view P.evEmployee) validations'

        let newMap = IdMap.fromFoldable employees
        let allData' = P.PersonioAllData employees validations cl clRole

-- Disable compacting for now to try to see if it effects personio-proxy stability
#if __GLASGOW_HASKELL__ >= 804
        allData <- do
            region0 <- compactWithSharing allData'
            size <- compactSize region0
            runLogT "compact" (ctxLogger ctx) $
                logInfoI "Compacted personio data $size" $ object [ "size" .= size ]
            return $ getCompact region0
#else
        allData <- evaluate $ force $ allData'
#endif
        liftIO $ runLogT "personio-proxy" (ctxLogger ctx) $ logInfo_ "Checking changes"

        !changed <- atomically $ do
            oldMap <- readTVar (ctxPersonioData ctx)
            writeTVar (ctxPersonioData ctx) allData
            return (comparePersonioList (IdMap.fromFoldable . P.paEmployees $ oldMap) newMap)

        if changed
        then runLogT "update" (ctxLogger ctx) $ logInfo_ "Personio updated: no changes"
        else do
            runLogT "update" (ctxLogger ctx) $ do
                logInfo_ "Personio updated: data changed"
--                logInfo "Personio updated: number of employee changed: " (length changed)
                -- Save in DB
                _ <- Postgres.safePoolExecute ctx insertQuery (Only $ toJSON employees)
                -- Tell the world
                liftIO $ publishMessage mq PersonioUpdated

notMachine :: P.Employee -> Bool
notMachine e = case e ^. P.employeeId of
    -- hardcoded here, these are ids of emailer accounts. they are no humans.
    P.EmployeeId 331863 -> False
    P.EmployeeId 386126 -> False
    P.EmployeeId 590516 -> False
    P.EmployeeId 656474 -> False
    P.EmployeeId 768834 -> False
    P.EmployeeId 768842 -> False
    _ -> True

comparePersonio
    :: IdMap P.Employee                    -- ^ old
    -> IdMap P.Employee                    -- ^ new
    -> Map P.EmployeeId (These P.Employee P.Employee) -- ^ "diff"
comparePersonio old new =
    Map.mapMaybe id $ alignWith f (IdMap.toMap old) (IdMap.toMap new)
  where
    f x = case x of
        These o n | o == n -> Nothing
        _                  -> Just x

comparePersonioList
    :: IdMap P.Employee
    -> IdMap P.Employee
    -> Bool--List P.EmployeeId
comparePersonioList old new =
    let employeeIds = Set.toList $ keysSet old <> keysSet new
        hasChanged eid = case (old ^. at eid, new ^. at eid) of
            (Just oldEmployee, Just newEmployee) -> if oldEmployee == newEmployee then Nothing else Just eid
            _ -> Just eid
        changed = fmap hasChanged employeeIds
    in null $ catMaybes changed
