{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.MegaRepoTool.PatternCompleter (
    patternCompleter,
    ) where

import Data.Maybe                              (isJust)
import Cabal.Plan
       (CompInfo (..), CompName (..), PkgId (..), PkgName (..), PlanJson (..),
       Unit (..), dispCompName, findAndDecodePlanJson)
import Futurice.Prelude
import Prelude ()

import qualified Data.Map            as Map
import qualified Options.Applicative as O
import qualified Data.Text           as T

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
