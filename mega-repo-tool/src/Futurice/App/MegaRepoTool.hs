{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.MegaRepoTool (defaultMain) where

import Data.Aeson                              (Value (Null))
import Futurice.Prelude
import Prelude ()
import Text.PrettyPrint.ANSI.Leijen.AnsiPretty (ansiPretty)

import qualified Data.Map            as Map
import qualified Options.Applicative as O
import qualified Text.Microstache    as M

import Futurice.App.MegaRepoTool.BuildDocker
import Futurice.App.MegaRepoTool.Config
import Futurice.App.MegaRepoTool.Estimator
import Futurice.App.MegaRepoTool.Scripts
import Futurice.App.MegaRepoTool.Stats


data Cmd
    = BuildDocker [AppName]
    | Action (IO ())
    | ViewConfig

buildDockerOptions ::O.Parser Cmd
buildDockerOptions = BuildDocker
    <$> some (O.strArgument $ mconcat
        [ O.metavar ":component"
        , O.help "Component/image to build"
        , O.completer $ O.listIOCompleter comp
        ])
  where
    comp :: IO [String]
    comp = mk <$> readConfig

    mk = map (view unpacked) . Map.keys . _mrtApps

packdepsOptions :: O.Parser Cmd
packdepsOptions = pure $ Action packdepsScript

dotOptions :: O.Parser Cmd
dotOptions = pure $ Action dotScript

statsOptions :: O.Parser Cmd
statsOptions =  pure $ Action stats

estimatorOptions :: O.Parser Cmd
estimatorOptions = fmap Action $ estimator
    <$> O.strArgument (mconcat [ O.metavar ":file", O.help "TODO File" ])

viewConfigOptions :: O.Parser Cmd
viewConfigOptions = pure ViewConfig

optsParser :: O.Parser Cmd
optsParser = O.subparser $ mconcat
    [ cmdParser "build-docker" buildDockerOptions "Build docker images"
    , cmdParser "packdeps" packdepsOptions "Run packdeps, i.e. check that dependency bounds allow newest versions"
    , cmdParser "dot" dotOptions "Update dependency graph image"
    , cmdParser "stats" statsOptions "Display some rough stats"
    , cmdParser "estimator" estimatorOptions "Calculate estimates"
    , cmdParser "view-config" viewConfigOptions "view config"
    ]
  where
    cmdParser :: String -> O.Parser Cmd -> String -> O.Mod O.CommandFields Cmd
    cmdParser cmd parser desc =
         O.command cmd $ O.info (O.helper <*> parser) $ O.progDesc desc

main' :: Cmd -> IO ()
main' (BuildDocker imgs) = buildDocker imgs
main' (Action x)         = x
main' ViewConfig         = do
    cfg <- readConfig
    print $ ansiPretty $ flip M.renderMustache Null <$> cfg

defaultMain :: IO ()
defaultMain = O.execParser opts >>= main'
  where
    opts = O.info (O.helper <*> optsParser) $ mconcat
        [ O.fullDesc
        , O.progDesc "Futurice haskell-mega-repo tool"
        , O.header "mega-repo-tool"
        ]
