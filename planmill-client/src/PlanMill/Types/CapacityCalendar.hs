{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.CapacityCalendar (
    CapacityCalendar(..),
    CapacityCalendars,
    CapacityCalendarId,
    ) where

import PlanMill.Internal.Prelude

import PlanMill.Types.Identifier (HasIdentifier (..), Identifier)

type CapacityCalendarId = Identifier CapacityCalendar
type CapacityCalendars = Vector CapacityCalendar

data CapacityCalendar = CapacityCalendar
    { _ccId                    :: !CapacityCalendarId
    , ccName                  :: !(Maybe Text)
    , ccDefaultDailyWorktime  :: !(Maybe (NDT 'Minutes Int))
{-
    , ccCountry               :: !(Maybe Int)
    , ccDefaultWeekStartDay   :: !(Maybe Int)
    , ccDefaultDailyStartTime :: !(Maybe Int)
    , ccStart                 :: !UTCTime
    , ccDefaultWeeklyWorkdays :: !(Maybe Int)
    , ccFinish                :: !UTCTime
    , ccActiveUsers           :: !(Maybe Int)
    , ccType                  :: !(Maybe Int)
-}
    }
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

makeLenses ''CapacityCalendar
deriveGeneric ''CapacityCalendar

instance HasKey CapacityCalendar where
    type Key CapacityCalendar = CapacityCalendarId
    key = ccId

instance HasIdentifier CapacityCalendar CapacityCalendar where
    identifier = ccId

instance Hashable CapacityCalendar
instance NFData CapacityCalendar
instance AnsiPretty CapacityCalendar
instance Binary CapacityCalendar
instance Structured CapacityCalendar

instance FromJSON CapacityCalendar where
    parseJSON = withObject "CapacityCalendar" $ \obj -> CapacityCalendar
        <$> obj .: "id"
        <*> obj .: "name"
        <*> obj .: "defaultDailyWorktime"
{-
        <*> obj .: "country"
        <*> obj .: "defaultWeekStartDay"
        <*> obj .: "defaultDailyStartTime"
        <*> (getU <$> obj .: "start")
        <*> obj .: "defaultWeeklyWorkdays"
        <*> (getU <$> obj .: "finish")
        <*> obj .: "activeUsers"
        <*> obj .: "type"
-}
