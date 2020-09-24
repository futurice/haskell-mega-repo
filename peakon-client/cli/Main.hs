{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Aeson         (object, (.=))
import Futurice.EnvConfig (getConfig)
import Futurice.Prelude
import Prelude ()

import Peakon.Eval
import Peakon.Request
import Peakon.Types

import qualified Data.Text           as T
import qualified Options.Applicative as O

data Cmd = CmdGetOverview
         | CmdGetEmployees

getOverviewOptions :: O.Parser Cmd
getOverviewOptions = pure CmdGetOverview

employeesOptions :: O.Parser Cmd
employeesOptions = pure CmdGetEmployees

optsParser :: O.Parser Cmd
optsParser = O.subparser $ mconcat
    [ cmdParser "get-overview" getOverviewOptions "Get overview"
    , cmdParser "employees" employeesOptions "Get employees"
    ]
  where
    cmdParser :: String -> O.Parser Cmd -> String -> O.Mod O.CommandFields Cmd
    cmdParser cmd parser desc =
         O.command cmd $ O.info (O.helper <*> parser) $ O.progDesc desc

strArgument :: IsString a => [O.Mod O.ArgumentFields a] -> O.Parser a
strArgument = O.strArgument . mconcat

argument :: O.ReadM a -> [O.Mod O.ArgumentFields a] -> O.Parser a
argument x y = O.argument x $ mconcat y

main' :: Logger -> PeakonCfg -> Cmd -> IO ()
main' lgr token CmdGetOverview = do
    mgr <- liftIO $ newManager tlsManagerSettings
    overview <- evalPeakonReqIO token mgr lgr ReqEngagementOverview
    print overview
    pure ()
main' lgr token CmdGetEmployees = do
    mgr <- liftIO $ newManager tlsManagerSettings
    employees <- evalPeakonReqIO token mgr lgr ReqEmployees
    print employees
    pure ()

main :: IO ()
main = withStderrLogger $ \lgr -> runLogT "peakon-cli" lgr $ do
    cfg <- getConfig "PEAKONCLIENT"
    liftIO $ (O.execParser opts >>= main' lgr cfg)
  where
    opts = O.info (O.helper <*> optsParser) $ mconcat
        [ O.fullDesc
        , O.progDesc "Cli for peakon-client"
        , O.header "peakon-cli"
        ]
