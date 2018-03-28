{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.MegaRepoTool (defaultMain) where

import Cabal.Plan
       (CompInfo (..), CompName (..), PkgId (..), PkgName (..), PlanJson (..),
       Unit (..), dispCompName, findAndDecodePlanJson)
import Data.Aeson                              (Value (Null))
import Data.Maybe                              (isJust)
import Futurice.Prelude
import Prelude ()
import Text.PrettyPrint.ANSI.Leijen.AnsiPretty (ansiPretty)

import qualified Data.Map            as Map
import qualified Data.Text           as T
import qualified Options.Applicative as O
import qualified Text.Microstache    as M

import Futurice.App.MegaRepoTool.BuildDocker
import Futurice.App.MegaRepoTool.Config
import Futurice.App.MegaRepoTool.Estimator
import Futurice.App.MegaRepoTool.Exec
import Futurice.App.MegaRepoTool.GenPass
import Futurice.App.MegaRepoTool.Keys
import Futurice.App.MegaRepoTool.Scripts
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

packdepsOptions :: O.Parser Cmd
packdepsOptions = pure $ CmdAction packdepsScript

dotOptions :: O.Parser Cmd
dotOptions = pure $ CmdAction dotScript

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

patternCompleter :: Bool -> O.Completer
patternCompleter onlyWithExes = O.mkCompleter $ \pfx -> do
    (plan, _) <- findAndDecodePlanJson Nothing
    let tpfx  = T.pack pfx
        components = findComponents plan

    {-
    -- One scenario
    --
    -- @
    -- $ cabal-plan list-bin cab<TAB>
    -- $ cabal-plan list-bin cabal-plan<TAB>
    -- $ cabal-plan list-bin cabal-plan:exe:cabal-plan
    -- @
    --
    -- Note: if this package had `tests` -suite, then we can
    --
    -- @
    -- $ cabal-plan list-bin te<TAB>
    -- $ cabal-plan list-bin tests<TAB>
    -- $ cabal-plan list-bin cabal-plan:test:tests
    -- @
    --
    -- *BUT* at least zsh script have to be changed to complete from non-prefix.
    -}
    return $ map T.unpack $ firstNonEmpty
        -- 1. if tpfx matches component exacty, return full path
        [ single $ map fst $ filter ((tpfx ==) . snd) components

        -- 2. match component parts
        , uniques $ filter (T.isPrefixOf tpfx) $ map snd components

        -- otherwise match full paths
        , filter (T.isPrefixOf tpfx) $ map fst components
        ]
  where
    firstNonEmpty :: [[a]] -> [a]
    firstNonEmpty []         = []
    firstNonEmpty ([] : xss) = firstNonEmpty xss
    firstNonEmpty (xs : _)   = xs

    -- single
    single :: [a] -> [a]
    single xs@[_] = xs
    single _      = []

    -- somewhat like 'nub' but drop duplicate names. Doesn't preserve order
    uniques :: Ord a => [a] -> [a]
    uniques = Map.keys . Map.filter (== 1) . Map.fromListWith (+) . map (\x -> (x, 1 :: Int))

    impl :: Bool -> Bool -> Bool
    impl False _ = True
    impl True  x = x

    -- returns (full, cname) pair
    findComponents :: PlanJson -> [(T.Text, T.Text)]
    findComponents plan = do
        (_, Unit{..}) <- Map.toList $ pjUnits plan
        (cn, ci) <- Map.toList $ uComps

        -- if onlyWithExes, component should have binFile
        guard (onlyWithExes `impl` isJust (ciBinFile ci))

        let PkgId pn@(PkgName pnT) _ = uPId
            g = case cn of
                CompNameLib -> pnT <> T.pack":lib:" <> pnT
                _           -> pnT <> T.pack":" <> dispCompName cn

        let cnT = extractCompName pn cn
        [ (g, cnT) ]

extractCompName :: PkgName -> CompName -> T.Text
extractCompName (PkgName pn) CompNameLib         = pn
extractCompName (PkgName pn) CompNameSetup       = pn
extractCompName _            (CompNameSubLib cn) = cn
extractCompName _            (CompNameFLib cn)   = cn
extractCompName _            (CompNameExe cn)    = cn
extractCompName _            (CompNameTest cn)   = cn
extractCompName _            (CompNameBench cn)  = cn

execOptions :: O.Parser Cmd
execOptions = mk
    <$> O.strArgument (O.metavar ":cmd" <> O.help "command")
    <*> many (O.strArgument $ O.metavar ":arg" <> O.help "arguments")
  where
    mk cmd args = CmdExec (cmd :| args)

strArgument :: IsString a => [O.Mod O.ArgumentFields a] -> O.Parser a
strArgument = O.strArgument . mconcat

optsParser :: O.Parser Cmd
optsParser = O.subparser $ mconcat
    [ cmdParser "build-docker" buildDockerOptions "Build docker images"
    , cmdParser "packdeps"     packdepsOptions    "Run packdeps, i.e. check that dependency bounds allow newest versions"
    , cmdParser "dot"          dotOptions         "Update dependency graph image"
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
    ]
  where
    cmdParser :: String -> O.Parser Cmd -> String -> O.Mod O.CommandFields Cmd
    cmdParser cmd parser desc =
         O.command cmd $ O.info (O.helper <*> parser) $ O.progDesc desc

main' :: Cmd -> IO ()
main' (CmdBuildDocker imgs) = buildDocker imgs
main' (CmdAction x)         = x
main' CmdGenPass            = cmdGenPass
main' CmdStackYaml          = stackYaml
main' CmdViewConfig         = do
    cfg <- readConfig
    print $ ansiPretty $ flip M.renderMustache Null <$> cfg
main' (CmdListKeys b)    = listKeys b
main' (CmdAddKey k v)    = addKey True k v
main' (CmdAddOwnKey k v) = addKey False k v
main' (CmdUpdateKey k v) = updateKey k v
main' (CmdDeleteKey k)   = deleteKey k
main' (CmdExec args)     = cmdExec args

defaultMain :: IO ()
defaultMain = O.execParser opts >>= main'
  where
    opts = O.info (O.helper <*> optsParser) $ mconcat
        [ O.fullDesc
        , O.progDesc "Futurice haskell-mega-repo tool"
        , O.header "mega-repo-tool"
        ]
