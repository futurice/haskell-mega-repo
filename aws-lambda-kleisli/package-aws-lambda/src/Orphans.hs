{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Orphans () where

import           Instances.TH.Lift          ()

import qualified Language.Haskell.TH.Syntax as TH
import qualified Text.Microstache           as Mu

deriving instance TH.Lift Mu.Key
deriving instance TH.Lift Mu.Node
deriving instance TH.Lift Mu.PName
deriving instance TH.Lift Mu.Template
