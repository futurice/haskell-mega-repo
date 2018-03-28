{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module PlanMill.Types.Hook (
    Hooks,
    HookId,
    Hook(..),
    HookType(..),
    NewHook(..),
) where

import qualified Options.SOP               as O
import           PlanMill.Internal.Prelude
import           PlanMill.Types.Identifier (Identifier (..))

-- Non-exhaustive list of possible hooks for testing purposes
data HookType = RequestAdd | TimeReportInsert
    deriving (Eq, Ord, Read, Show, Generic, Typeable)

makeLenses ''HookType
deriveGeneric ''HookType
instance Hashable HookType
instance NFData HookType
instance AnsiPretty HookType
instance ToJSON HookType where
    toJSON RequestAdd = String "request.add"
    toJSON TimeReportInsert = String "timereport.insert"

instance O.FromOptions HookType where
    optionsParser = O.argument O.auto (O.metavar ":???")

data Hook = Hook
    { _hId :: !HookId
    , _hHook :: !Text
    , _hUrl :: !Text
    , _hEventUser :: !(Maybe Int)
    , _hEventProject :: !(Maybe Int)
    }
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

type HookId = Identifier Hook
type Hooks = Vector Hook

instance Hashable Hook
instance NFData Hook
instance AnsiPretty Hook
instance ToJSON Hook
instance FromJSON Hook where
    parseJSON = withObject "Hook" $ \obj -> Hook
        <$>  obj  .:  "id"
        <*>  obj  .:  "hook"
        <*>  obj  .:  "url"
        <*>  obj  .:  "eventUser"
        <*>  obj  .:  "eventProject"

makeLenses ''Hook
deriveGeneric ''Hook

data NewHook = NewHook
    { _nhHook :: !HookType
    , _nhUrl :: !Text
    }
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

instance Hashable NewHook
instance NFData NewHook
instance AnsiPretty NewHook

makeLenses ''NewHook
deriveGeneric ''NewHook

instance ToJSON NewHook where
    toJSON NewHook {..} = object
        [ "hook"    .= _nhHook
        , "url"     .= _nhUrl
        ]

