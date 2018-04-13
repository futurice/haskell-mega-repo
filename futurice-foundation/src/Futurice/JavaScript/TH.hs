{-# LANGUAGE TemplateHaskell #-}
module Futurice.JavaScript.TH (
    embedJS,
    ) where

import FileEmbedLzma       (embedText)
import Futurice.JavaScript
import Futurice.Prelude
import Language.Haskell.TH (Exp, Q, runIO)
import Prelude ()

import qualified Data.Text.IO as T

-- | Create 'JS' from a file, compile-time verifying it can be parsed.
--
-- > $(embedJS "supersource.js")
embedJS :: FilePath -> Q Exp
embedJS fp = do
    contents <- runIO $ T.readFile fp
    case makeJS contents fp of
        Left  err -> fail $ "embedJS " <> fp <> " -- " <> err
        Right _js -> [| unsafeMakeJS $(embedText fp) |]
