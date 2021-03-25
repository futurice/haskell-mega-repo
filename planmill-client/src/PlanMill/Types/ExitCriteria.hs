{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.ExitCriteria (
    ExitCriteria(..),
    ) where

import PlanMill.Internal.Prelude

data ExitCriteria = ExitCriteria
    { ecName        :: !Text
    , ecBoardlist   :: !Text
    , ecDescription :: !Text
    , ecId          :: !Text
    }
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

deriveGeneric ''ExitCriteria

instance Hashable ExitCriteria
instance NFData ExitCriteria
instance AnsiPretty ExitCriteria
instance Binary ExitCriteria
instance Structured ExitCriteria

instance FromJSON ExitCriteria where
    parseJSON = withObject "ExitCriteria" $ \obj ->
        ExitCriteria <$> obj .: "name"
                     <*> obj .: "boardlist"
                     <*> obj .: "description"
                     <*> obj .: "id"
