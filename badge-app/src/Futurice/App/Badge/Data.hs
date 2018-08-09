{-# LANGUAGE TemplateHaskell #-}
module Futurice.App.Badge.Data where

import FileEmbedLzma
import Futurice.Prelude
import Prelude ()

tarContents :: LazyByteString
tarContents = $(embedLazyByteString "data.tar")
