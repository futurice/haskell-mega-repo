{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.Lambda.PlanMillProxy.Accounts (planMillProxyAccountsUpdate) where

import Futurice.EnvConfig
import Futurice.Lambda
import Futurice.Postgres
       (ConnectInfo, HasPostgresPool, createPostgresPool, safePoolExecute)
import Futurice.Prelude
import Prelude ()

import qualified PlanMill as PM

data Config = Config
    { cfgPmCfg            :: !PM.Cfg
    , cfgPostgresConnInfo :: !ConnectInfo
    }
  deriving (Show)

instance Configure Config where
    configure = Config
        <$> configure
        <*> envConnectInfo

planMillProxyAccountsUpdate :: AwsLambdaHandler
planMillProxyAccountsUpdate = makeAwsLambda impl
  where
     impl :: LambdaContext -> AWSEnv -> Config -> Logger -> Manager -> Value -> LogT IO ()
     impl lc _ Config {..} lgr mgr value = do
         pool <- createPostgresPool cfgPostgresConnInfo
         now <- currentTime

         logInfoI "Planmill account lambda got webhook value " $ value

         pure ()
