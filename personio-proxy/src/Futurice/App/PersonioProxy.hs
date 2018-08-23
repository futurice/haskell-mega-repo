{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.PersonioProxy (defaultMain) where

import Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar)
import Data.Aeson.Types       (object, parseEither, parseJSON, toJSON, (.=))
import Futurice.IdMap         (IdMap)
import Futurice.Periocron
import Futurice.Prelude
import Futurice.Servant
import Futurice.Time
import Prelude ()
import Servant                ((:<|>) (..), Server)

import Futurice.App.PersonioProxy.API
import Futurice.App.PersonioProxy.Config
import Futurice.App.PersonioProxy.Logic
import Futurice.App.PersonioProxy.Types

import qualified Data.Map.Strict   as Map
import qualified Futurice.IdMap    as IdMap
import qualified Futurice.Postgres as Postgres
import qualified Personio          as P

server :: Ctx -> Server PersonioProxyAPI
server ctx = pure "Try /swagger-ui/"
    :<|> personioRequest ctx
    :<|> rawEmployees ctx
    :<|> scheduleEmployees ctx
    :<|> employeesChart ctx
    :<|> tribeEmployeesChart ctx
    :<|> rolesDistributionChart ctx

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
    -> Postgres.Pool Postgres.Connection
    -> IdMap P.Employee
    -> [P.EmployeeValidation]
    -> IO Ctx
newCtx lgr cache pool es vs = do
    today <- currentDay
    let active = Map.singleton today $ P.internSimpleEmployees traverse $
            es ^.. folded . P.simpleEmployee
    Ctx lgr cache pool
        <$> newTVarIO es
        <*> newTVarIO vs
        <*> newTVarIO active

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
            (Postgres.Only v : _) -> case parseEither parseJSON v of
                Right employees -> return employees
                Left err        -> do
                    logAttention "Cannot parse previous Personio data" err
                    return []

    -- context
    ctx <- newCtx lgr cache pool (IdMap.fromFoldable employees) []

    -- jobs
    let fetchEmployees = P.evalPersonioReqIO mgr lgr cfg P.PersonioAll
    let intervalSec = unNDT (ndtConvert' intervalMin :: NDT 'Seconds NominalDiffTime) -- TODO: add this to futurice-prelude
    let employeesJob   = mkJob "Update personio data" (updateJob ctx fetchEmployees) $ every intervalSec

    pure (ctx, [ employeesJob ])
  where
    updateJob :: Ctx -> IO ([P.Employee], [P.EmployeeValidation]) -> IO ()
    updateJob ctx fetchEmployees = do
        (employees', validations') <- fetchEmployees
        let employees = filter notMachine employees'
        let validations = filter (notMachine . view P.evEmployee) validations'

        changed <- atomically $ do
            oldMap <- readTVar (ctxPersonio ctx)
            let newMap = IdMap.fromFoldable employees
            writeTVar (ctxPersonio ctx) newMap
            writeTVar (ctxPersonioValidations ctx) validations
            return (comparePersonio oldMap newMap)
                
        -- Update active
        runLogT "simple-employees" (ctxLogger ctx) $ do
            res <- Postgres.safePoolQuery_ ctx "SELECT DISTINCT ON (timestamp :: date) timestamp, contents FROM \"personio-proxy\".log;"
            let ses = P.internSimpleEmployees (traverse . traverse) $ Map.fromList
                  [ (d, ids)
                  | (t, v) <- res
                  , let d = utctDay t
                  , let ids = case parseEither parseJSON v of
                          Left _   -> mempty
                          Right es -> (es :: [P.Employee]) ^.. folded . P.simpleEmployee
                  ]
            logInfoI "Updating active list: count $n" $ object
                [ "n" .= length ses
                ]
            liftIO $ atomically $ writeTVar (ctxSimpleEmployees ctx) ses

        unless (null changed) $ do
            runLogT "update" (ctxLogger ctx) $ do
                logInfo "Personio updated, data changed" changed
                -- Save in DB
                _ <- Postgres.safePoolExecute ctx insertQuery (Postgres.Only $ toJSON employees)
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
    _      -> True

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
