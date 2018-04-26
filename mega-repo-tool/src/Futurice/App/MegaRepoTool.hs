{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.MegaRepoTool (defaultMain) where

import Data.Aeson                              (Value (Null))
import Futurice.Prelude
import Prelude ()
import Text.PrettyPrint.ANSI.Leijen.AnsiPretty (ansiPretty)

import qualified Data.Map            as Map
import qualified Options.Applicative as O
import qualified Text.Microstache    as M

import Futurice.App.MegaRepoTool.CopyArtifacts
import Futurice.App.MegaRepoTool.BuildDocker
import Futurice.App.MegaRepoTool.Config
import Futurice.App.MegaRepoTool.Estimator
import Futurice.App.MegaRepoTool.Exec
import Futurice.App.MegaRepoTool.GenPass
import Futurice.App.MegaRepoTool.Keys
import Futurice.App.MegaRepoTool.PatternCompleter
import Futurice.App.MegaRepoTool.StackYaml
import Futurice.App.MegaRepoTool.Stats

data Cmd
    = CmdBuildDocker [AppName]
    | CmdAction (IO ())
    | CmdViewConfig
    | CmdGenPass
    | CmdStackYaml
    | CmdListKeys !Bool
    | CmdAddKey !Text !Text
    | CmdAddOwnKey !Text !Text
    | CmdUpdateKey !Text !Text
    | CmdDeleteKey !Text
    | CmdExec !(NonEmpty String)
    | CmdCopyArtifacts FilePath FilePath

buildDockerOptions ::O.Parser Cmd
buildDockerOptions = CmdBuildDocker
    <$> some (O.strArgument $ mconcat
        [ O.metavar ":component"
        , O.help "Component/image to build"
        , O.completer $ O.listIOCompleter comp
        ])
  where
    comp :: IO [String]
    comp = mk <$> readConfig

    mk = map (view unpacked) . Map.keys . _mrtApps

statsOptions :: O.Parser Cmd
statsOptions =  pure $ CmdAction stats

estimatorOptions :: O.Parser Cmd
estimatorOptions = fmap CmdAction $ estimator
    <$> strArgument [ O.metavar ":file", O.help "TODO File" ]

viewConfigOptions :: O.Parser Cmd
viewConfigOptions = pure CmdViewConfig

genPassOptions :: O.Parser Cmd
genPassOptions = pure CmdGenPass

stackYamlOptions :: O.Parser Cmd
stackYamlOptions = pure CmdStackYaml

listKeysOptions :: O.Parser Cmd
listKeysOptions = CmdListKeys
    <$> O.switch (mconcat [O.long "show-values", O.help "Show the values or keys"])

addKeyOptions :: O.Parser Cmd
addKeyOptions = CmdAddKey
    <$> strArgument [ O.metavar ":key", O.help "Key identifier" ]
    <*> strArgument [ O.metavar ":value", O.help "Key value" ]

addOwnKeyOptions :: O.Parser Cmd
addOwnKeyOptions = CmdAddOwnKey
    <$> strArgument [ O.metavar ":key", O.help "Key identifier" ]
    <*> strArgument [ O.metavar ":value", O.help "Key value" ]

updateKeyOptions :: O.Parser Cmd
updateKeyOptions = CmdUpdateKey
    <$> strArgument [ O.metavar ":key", O.help "Key identifier" ]
    <*> strArgument [ O.metavar ":value", O.help "Key value" ]

deleteKeyOptions :: O.Parser Cmd
deleteKeyOptions = CmdDeleteKey
    <$> strArgument [ O.metavar ":key", O.help "Key identifier" ]

runOptions :: O.Parser Cmd
runOptions = cmdRun
    <$> exe
    <*> many (O.strArgument $ O.metavar ":arg" <> O.help "arguments")
  where
    cmdRun e args = CmdExec ("cabal" :| "new-run" : e : "--" : args)

    exe = O.strArgument $ mconcat
        [ O.metavar "exe"
        , O.help "Patterns to match."
        , O.completer $ patternCompleter True
        ]

execOptions :: O.Parser Cmd
execOptions = mk
    <$> O.strArgument (O.metavar ":cmd" <> O.help "command")
    <*> many (O.strArgument $ O.metavar ":arg" <> O.help "arguments")
  where
    mk cmd args = CmdExec (cmd :| args)

copyArtifactsOptions :: O.Parser Cmd
copyArtifactsOptions = CmdCopyArtifacts
    <$> O.strOption (O.long "rootdir"  <> O.metavar ":dir" <> O.help "The root directory")
    <*> O.strOption (O.long "builddir" <> O.metavar ":dir" <> O.help "The directory where Cabal put build files")

strArgument :: IsString a => [O.Mod O.ArgumentFields a] -> O.Parser a
strArgument = O.strArgument . mconcat

optsParser :: O.Parser Cmd
optsParser = O.subparser $ mconcat
    [ cmdParser "build-docker" buildDockerOptions "Build docker images"
    , cmdParser "stats"        statsOptions       "Display some rough stats"
    , cmdParser "estimator"    estimatorOptions   "Calculate estimates"
    , cmdParser "view-config"  viewConfigOptions  "view config"
    , cmdParser "gen-pass"     genPassOptions     "Generate passwords"
    , cmdParser "stack-yaml"   stackYamlOptions   "Generate stack.yaml from cabal.project"
    , cmdParser "list-keys"    listKeysOptions    "List all keys"
    , cmdParser "add-key"      addKeyOptions      "Add new key"
    , cmdParser "add-own-key"  addOwnKeyOptions   "Add own new key (not shared with others)"
    , cmdParser "update-key"   updateKeyOptions   "Update existing key"
    , cmdParser "delete-key"   deleteKeyOptions   "Delete key"
    , cmdParser "run"          runOptions         "Run command through cabal new-run"
    , cmdParser "exec"         execOptions        "Execute command in environment"
    , cmdParser "copy-artifacts" copyArtifactsOptions "(Internal) copy-artifacts during build"
    ]
  where
    cmdParser :: String -> O.Parser Cmd -> String -> O.Mod O.CommandFields Cmd
    cmdParser cmd parser desc =
         O.command cmd $ O.info (O.helper <*> parser) $ O.progDesc desc

main' :: Cmd -> IO ()
main' (CmdBuildDocker imgs)    = buildDocker imgs
main' (CmdAction x)            = x
main' CmdGenPass               = cmdGenPass
main' CmdStackYaml             = stackYaml
main' CmdViewConfig            = do
    cfg <- readConfig
    print $ ansiPretty $ flip M.renderMustache Null <$> cfg
main' (CmdListKeys b)          = listKeys b
main' (CmdAddKey k v)          = addKey True k v
main' (CmdAddOwnKey k v)       = addKey False k v
main' (CmdUpdateKey k v)       = updateKey k v
main' (CmdDeleteKey k)         = deleteKey k
main' (CmdExec args)           = cmdExec args
main' (CmdCopyArtifacts rd bd) = cmdCopyArtifacts rd bd

defaultMain :: IO ()
defaultMain = O.execParser opts >>= main'
  where
    opts = O.info (O.helper <*> optsParser) $ mconcat
        [ O.fullDesc
        , O.progDesc "Futurice haskell-mega-repo tool"
        , O.header "mega-repo-tool"
        ]
