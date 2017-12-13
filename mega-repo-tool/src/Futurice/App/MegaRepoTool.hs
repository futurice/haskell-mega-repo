{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.MegaRepoTool (defaultMain) where

import Data.Yaml        (decodeFileEither)
import Futurice.Prelude
import Prelude ()

import qualified Options.Applicative as O

import Futurice.App.MegaRepoTool.Command.BuildDocker
import Futurice.App.MegaRepoTool.Command.ListSnapshotDependencies
import Futurice.App.MegaRepoTool.Config
import Futurice.App.MegaRepoTool.Estimator
import Futurice.App.MegaRepoTool.Scripts
import Futurice.App.MegaRepoTool.Stats

import qualified Data.Map as Map

data Cmd
    = ListSnapshotDependencies
    | BuildDocker [AppName]
    | Action (IO ())

listSnapshotDependenciesOptions :: O.Parser Cmd
listSnapshotDependenciesOptions = pure ListSnapshotDependencies

buildDockerOptions ::O.Parser Cmd
buildDockerOptions = BuildDocker
    <$> some (O.strArgument $ mconcat
        [ O.metavar ":component"
        , O.help "Component/image to build"
        , O.completer $ O.listIOCompleter comp
        ])
  where
    comp :: IO [String]
    comp = either (const []) mk <$> decodeFileEither "mega-repo-tool.yaml"

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

optsParser :: O.Parser Cmd
optsParser = O.subparser $ mconcat
    [ cmdParser "build-docker" buildDockerOptions "Build docker images"
    , cmdParser "list-snapshot-dependencies" listSnapshotDependenciesOptions "List snapshot dependencies (like stack list-dependencies)"
    , cmdParser "packdeps" packdepsOptions "Run packdeps, i.e. check that dependency bounds allow newest versions"
    , cmdParser "dot" dotOptions "Update dependency graph image"
    , cmdParser "stats" statsOptions "Display some rough stats"
    , cmdParser "estimator" estimatorOptions "Calculate estimates"
    ]
  where
    cmdParser :: String -> O.Parser Cmd -> String -> O.Mod O.CommandFields Cmd
    cmdParser cmd parser desc =
         O.command cmd $ O.info (O.helper <*> parser) $ O.progDesc desc

main' :: Cmd -> IO ()
main' ListSnapshotDependencies = listSnapshotDependencies
main' (BuildDocker imgs)       = buildDocker imgs
main' (Action x)               = x

defaultMain :: IO ()
defaultMain = O.execParser opts >>= main'
  where
    opts = O.info (O.helper <*> optsParser) $ mconcat
        [ O.fullDesc
        , O.progDesc "Futurice haskell-mega-repo tool"
        , O.header "mega-repo-tool"
        ]
