{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Main (main) where

import Control.Lens          (taking)
import Control.Monad.Http    (MonadHttp (..))
import Data.Constraint
import Futurice.CryptoRandom (CryptoGenError)
import Futurice.EnvConfig    (getConfig)
import Futurice.Prelude
import Futurice.Trans.PureT
import Log.Class             (MonadLog (..))
import PlanMill.Eval         (evalPlanMill)
import Prelude ()

import Control.Concurrent.Async.Lifted.Safe    (Concurrently (..))
import Text.PrettyPrint.ANSI.Leijen.AnsiPretty
       (AnsiPretty (..), linebreak, putDoc)

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Client  as H
import qualified Options.SOP          as O

import qualified PlanMill as PM

import Data.Reflection (reifySymbol)
import GHC.TypeLits    (KnownSymbol)

-------------------------------------------------------------------------------
-- Cmd
-------------------------------------------------------------------------------

data Cmd
    = CmdMe
    | CmdHooks
    | CmdHookDelete PM.HookId
    | CmdHookAdd PM.HookType Text
    | CmdUsers
    | CmdTeams
    | CmdUser PM.UserId
    | CmdUserMany PM.UserId Int
    | CmdTimereports PM.UserId (PM.Interval Day)
    | CmdCapacity PM.UserId (PM.Interval Day)
    | CmdTimereport PM.TimereportId
    | CmdAssignments PM.ProjectId
    | CmdReportableAssignments PM.UserId
    | CmdBalance PM.UserId
    | CmdTask PM.TaskId
    | CmdMeta Text
    | CmdEnumeration Text
    | CmdProjects
    | CmdProject PM.ProjectId
    | CmdAccounts
    | CmdAccount PM.AccountId
    | CmdAbsencesInterval (PM.Interval Day)
    | CmdReports
    | CmdAllRevenuesReport
    | CmdPortfolios
    | CmdValueCreationPerMonth

deriveGeneric ''Cmd

-------------------------------------------------------------------------------
-- Opts
-------------------------------------------------------------------------------

data Opts = Opts
    { optsDumpJson :: !Bool
    , optsDumpRaw  :: !Bool
    , optsShowAll  :: !Bool
    , optsTimeout  :: !Int
    }
  deriving Show

defaultOpts :: Opts
defaultOpts = Opts False False False 30

-------------------------------------------------------------------------------
-- Ctx
-------------------------------------------------------------------------------

data Ctx = Ctx
    { _ctxCryptoPool  :: !CryptoPool
    , _ctxPlanmillCfg :: !PM.Cfg
    , _ctxHttpManager :: !Manager
    , _ctxOpts        :: !Opts
    }

makeLenses ''Ctx

instance HasHttpManager Ctx where getHttpManager = view ctxHttpManager
instance PM.HasPlanMillCfg Ctx where planmillCfg = ctxPlanmillCfg
instance HasCryptoPool Ctx where cryptoPool = ctxCryptoPool

-------------------------------------------------------------------------------
-- Cli
-------------------------------------------------------------------------------

optsP :: O.Parser Opts
optsP = Opts
    <$> O.flag False True (O.long "dump-json" <> O.help "Print json")
    <*> O.flag False True (O.long "dump-raw" <> O.help "Print raw reqsponse")
    <*> O.flag False True (O.long "show-all" <> O.help "Show all entries, default: 10")
    <*> O.option O.auto (O.long "timeout" <> O.value 30 <> O.help "Timeout" <> O.showDefault)

main :: IO ()
main = withStderrLogger $ \lgr -> runLogT "pm-cli" lgr $ do
    (opts, cmd) <- liftIO $ O.execParser optsP'
    cfg <- getConfig "PM"
    gpool <- mkCryptoPool 2 (120 :: NominalDiffTime) 2
    mgr <- liftIO $ newManager tlsManagerSettings
        { H.managerResponseTimeout = H.responseTimeoutMicro $ optsTimeout opts * 1000000
        }
    execute opts cmd $ Ctx
        { _ctxPlanmillCfg = cfg
        , _ctxCryptoPool  = gpool
        , _ctxOpts        = defaultOpts
        , _ctxHttpManager = mgr
        }
  where
    optsP' = O.info (O.helper <*> ((,) <$> optsP <*> O.sopCommandParser)) $ mconcat
        [ O.fullDesc
        , O.progDesc "Planmill Client"
        , O.header "Let's see what it returns"
        ]

-------------------------------------------------------------------------------
-- Execution
-------------------------------------------------------------------------------

execute :: Opts -> Cmd -> Ctx -> LogT IO ()
execute opts cmd ctx = flip runPureT ctx { _ctxOpts = opts } $ runM $ case cmd of
    CmdMe -> do
        x <- PM.planmillAction PM.me
        putPretty x
    CmdHooks -> do
        x <- PM.planmillAction PM.hooks
        putPretty $ "Result size: " <> textShow (length x)
        putPretty $ if optsShowAll opts
            then x ^.. folded
            else x ^.. taking 10 traverse
    CmdHookDelete hid -> do
        x <- PM.planmillAction $ PM.deleteHook hid
        pure x
    CmdHookAdd ht url -> do
        x <- PM.planmillAction $ PM.addHook $ PM.NewHook ht url
        putPretty $ x ^.. folded
    CmdUsers -> do
        x <- PM.planmillAction PM.users
        putPretty $ "Result size: " <> textShow (length x)
        putPretty $ if optsShowAll opts
            then x ^.. folded
            else x ^.. taking 10 traverse
    CmdTeams -> do
        x <- PM.planmillAction PM.teams
        putPretty $ if optsShowAll opts
            then x ^.. folded
            else x ^.. taking 10 traverse
    CmdUser uid -> do
        x <- PM.planmillAction $ PM.user uid
        putPretty x
    CmdUserMany uid n -> do
        -- xs <- for [1..n] $ \_ -> PM.planmillAction $ PM.user uid
        xs <- runConcurrently $ for [1..n] $ \_ -> Concurrently $ do
            PM.planmillAction $ PM.user uid
        case xs of
            [] -> pure ()
            (x:_) -> do
                putPretty x
                -- Check that all are the same:
                liftIO $ print $ all (==x) xs
    CmdAccounts -> do
        x <- PM.planmillAction PM.accounts
        putPretty $ if optsShowAll opts
            then x ^.. folded
            else x ^.. taking 10 traverse
    CmdAccount aid -> do
        x <- PM.planmillAction $ PM.account aid
        putPretty x
    CmdBalance uid -> do
        x <- PM.planmillAction $ PM.userTimeBalance uid
        putPretty x
    CmdTimereports uid interval -> do
        x <- PM.planmillAction $ PM.timereportsFromIntervalFor
            (PM.ResultInterval PM.IntervalStart interval)
            uid
        putPretty $ if optsShowAll opts
            then x ^.. folded
            else x ^.. taking 10 traverse
    CmdCapacity uid interval -> do
        x <- PM.planmillAction $ PM.userCapacity interval uid
        putPretty $ if optsShowAll opts
            then x ^.. folded
            else x ^.. taking 10 traverse
    CmdReportableAssignments uid -> do
        x <- PM.planmillAction $ PM.reportableAssignments uid
        putPretty $ if optsShowAll opts
            then x ^.. folded
            else x ^.. taking 10 traverse
    CmdAssignments pid -> do
        x <- PM.planmillAction $ PM.projectAssignments pid
        putPretty $ if optsShowAll opts
            then x ^.. folded
            else x ^.. taking 10 traverse
    CmdTask tid -> do
        x <- PM.planmillAction $ PM.task tid
        putPretty x
    CmdTimereport trid -> do
        x <- PM.planmillAction $ PM.timereport trid
        putPretty x
    CmdMeta path  -> do
        x <- PM.planmillAction $ PM.planMillGet path
        putPretty (x :: PM.Meta)
    CmdProjects -> do
        x <- PM.planmillAction $ PM.projects
        putPretty x
    CmdProject pid -> do
        x <- PM.planmillAction $ PM.project pid
        putPretty x
    CmdEnumeration enum -> reifyTextSymbol enum $ \p -> do
        x <- PM.planmillAction $ PM.enumerations p
        putPretty x
    CmdAbsencesInterval interval -> do
        x <- PM.planmillAction $ PM.absencesFromInterval
            (PM.ResultInterval PM.IntervalStart interval)
        putPretty $ if optsShowAll opts
            then x ^.. folded
            else x ^.. taking 10 traverse
    CmdReports -> do
        x <- PM.planmillAction PM.reports
        putPretty x
    CmdAllRevenuesReport -> do
        x <- PM.planmillAction PM.allRevenuesReport
        putPretty x
    CmdPortfolios -> do
        x <- PM.planmillAction PM.portfolios
        putPretty x
    CmdValueCreationPerMonth -> do
        x <- PM.planmillAction PM.valueCreationByMonthReport
        putPretty x
-------------------------------------------------------------------------------
-- M - monad with custom instances
-------------------------------------------------------------------------------

newtype M a = M { runM :: PureT CryptoGenError Ctx (LogT IO) a }
  deriving
    ( Functor, Applicative, Monad
    , MonadThrow, MonadTime, MonadError CryptoGenError, MonadClock
    , PM.MonadCRandom CryptoGenError
    , MonadReader Ctx
    )

instance MonadIO M where
    liftIO = liftBase

instance MonadBase IO M where
    liftBase = M . lift . lift

instance MonadBaseControl IO M where
    type StM M a = a
    liftBaseWith f = M (liftBaseWith (\g -> f (g . runM)))
    restoreM st  = M (restoreM st)

instance PM.MonadPlanMillConstraint M where
    type MonadPlanMillC M = Aeson.FromJSON
    entailMonadPlanMillCVector _ _    = Sub Dict

instance PM.MonadPlanMill M where
    planmillAction = evalPlanMill

instance MonadHttp M where
    httpLbs req = M $ do
        opts <- view ctxOpts
        res <- httpLbs req
        when (optsDumpRaw opts) $ liftIO $ do
            LBS.putStr (H.responseBody res)
            putChar '\n'
        when (optsDumpJson opts) $
            for_ (Aeson.decode (H.responseBody res) :: Maybe Value) putPretty
        pure res

-- | not necessary to do this way;
-- we can be PureT _ _ IO directly, with bigger Ctx
instance MonadLog M where
    logMessage t x l d = M (lift (logMessage t x l d))
    localData d (M (PureT m)) = M (PureT $ \r -> localData d (m r))
    localDomain d (M (PureT m)) = M (PureT $ \r -> localDomain d (m r))

-------------------------------------------------------------------------------
-- putPretty
-------------------------------------------------------------------------------

putPretty :: (MonadIO m, AnsiPretty a) => a -> m ()
putPretty = liftIO . putDoc . (<> linebreak) . ansiPretty

-------------------------------------------------------------------------------
-- move to futurice-prelude
-------------------------------------------------------------------------------

reifyTextSymbol :: forall r. Text -> (forall n. KnownSymbol n => Proxy n -> r) -> r
reifyTextSymbol t = reifySymbol (t ^. unpacked)
