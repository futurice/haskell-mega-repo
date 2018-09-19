{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.MegaRepoTool.StackYaml (stackYaml) where

import Cabal.Plan
       (FlagName, PkgId (..), PkgName, PlanJson (..), Unit (..), UnitType (..),
       findAndDecodePlanJson, SearchPlanJson (..))
import Control.Monad.Trans.State (StateT, execStateT, modify')
import Data.Aeson                (ToJSON (..), object, (.=))
import Data.List                 (elemIndex)
import Data.Yaml.Pretty          (defConfig, encodePretty, setConfCompare)
import Futurice.Prelude
import Prelude ()

import qualified Data.ByteString            as BS
import qualified Data.Map                   as Map
import qualified Data.Text                  as T
import qualified Distribution.Parsec.Common as P
import qualified Distribution.Parsec.Parser as P

-------------------------------------------------------------------------------
-- stack.yaml
-------------------------------------------------------------------------------

-- | Simplified @stack.yaml"
data StackYaml = StackYaml
    { syResolver  :: PkgId         -- ^ resolver is always a ghc version
    , syPackages  :: [FilePath]
    , syExtraDeps :: [PkgId]
    , syFlags     :: Map PkgName (Map FlagName Bool)
    }
  deriving (Show)

instance ToJSON StackYaml where
    toJSON sy = object
        [ "resolver"    .= syResolver sy
        , "packages"    .= syPackages sy
        , "extra-deps"  .= syExtraDeps sy
        , "flags"       .= syFlags sy
        , "allow-newer" .= True
        ]

-------------------------------------------------------------------------------
-- cabal.project
-------------------------------------------------------------------------------

readPackages :: IO [FilePath]
readPackages = do
    contents <- BS.readFile "cabal.project"
    either fail pure $ do
        fs <- first show $ P.readFields contents
        execStateT (traverse field fs) []
  where
    field :: P.Field P.Position -> StateT [FilePath] (Either String) ()
    field (P.Field (P.Name _ name) fls)
        | name == "packages" =
            modify' (++ words (fls' ^. unpacked))
      where
        fls' = T.unlines $ map (decodeUtf8Lenient . fieldLineContents) fls
    field _ = pure ()

fieldLineContents :: P.FieldLine ann -> ByteString
fieldLineContents (P.FieldLine _ bs) = bs

-------------------------------------------------------------------------------
-- action
-------------------------------------------------------------------------------

stackYaml :: IO ()
stackYaml = do
    pkgs <- readPackages
    plan <- findAndDecodePlanJson $ ProjectRelativeToDir "."
    let sy = StackYaml
            { syResolver  = pjCompilerId plan
            , syPackages  = sort pkgs
            , syExtraDeps = extraDeps plan
            , syFlags     = flags plan
            }
    -- TODO: configurable
    BS.writeFile "stack.yaml" (encodePretty cfg sy)
  where
    extraDeps plan = nub . sort $
        [ uPId unit
        | unit <- Map.elems (pjUnits plan)
        , uType unit `elem` [UnitTypeGlobal, UnitTypeInplace]
        ]

    flags :: PlanJson -> Map PkgName (Map FlagName Bool)
    flags plan = Map.filter (not . null) $ Map.fromListWith (<>)
        [ (uPkgName unit, uFlags unit)
        | unit <- Map.elems (pjUnits plan)
        ]

    uPkgName :: Unit -> PkgName
    uPkgName unit = case uPId unit of
        PkgId name _ -> name

    cfg = setConfCompare cmp defConfig
    cmp = keyOrder ["resolver", "packages", "extra-deps", "flags", "git", "commit", "location", "extra-dep"] <> compare

keyOrder :: [Text] -> Text -> Text -> Ordering
keyOrder xs a b = case (elemIndex a xs, elemIndex b xs) of
    (Just i,  Just j)   -> compare i j
    (Just _,  Nothing)  -> LT
    (Nothing, Just _)   -> GT
    (Nothing, Nothing)  -> EQ
