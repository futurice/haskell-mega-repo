{-# LANGUAGE RankNTypes #-}
module Futurice.KleeneSwagger (
    RE, sym, psym, msym, anySym,
    operationsMatching,
    ) where

import Control.Lens           (indices, itraversed)
import Data.List.Split        (splitOn)
import Data.Maybe             (isJust)
import Futurice.Prelude
import Prelude ()
import Text.Regex.Applicative (RE, anySym, match, msym, psym, sym)

import qualified Data.Swagger as Sw

operationsMatching :: RE String a -> Traversal' Sw.Swagger Sw.Operation
operationsMatching re =
    Sw.paths . itraversed . indices (isJust .match re . safeTail . splitOn "/") . pathItems
  where
    safeTail []     = []
    safeTail (_:xs) = xs

    pathItems :: Traversal' Sw.PathItem Sw.Operation
    pathItems f (Sw.PathItem piGet piPut piPost piDelete pipitions piHead piPatch piParams)
        = Sw.PathItem
            <$> traverse f piGet
            <*> traverse f piPut
            <*> traverse f piPost
            <*> traverse f piDelete
            <*> traverse f pipitions
            <*> traverse f piHead
            <*> traverse f piPatch
            <*> pure piParams

