module Futurice.App.Badge.Ctx where

import Data.Void        (Void)
import Futurice.Prelude
import Prelude ()

import qualified Codec.Archive.Tar as Tar

import Futurice.App.Badge.Config

data Ctx = Ctx
    { ctxConfig  :: Config
    , ctxLogger  :: Logger
    , ctxManager :: Manager
    , ctxTar     :: Tar.Entries Void
    }
