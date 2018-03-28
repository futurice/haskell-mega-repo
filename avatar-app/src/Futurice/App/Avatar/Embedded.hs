{-# LANGUAGE TemplateHaskell #-}
module Futurice.App.Avatar.Embedded where

import qualified Data.ByteString as BS
import           Data.FileEmbed  (embedFile, makeRelativeToProject)

futulogoBS :: BS.ByteString
futulogoBS = $(makeRelativeToProject "futulogo.png" >>= embedFile)
