{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.Timereport (
    Timereport(..),
    Timereports,
    TimereportId,
    NewTimereport(..),
    EditTimereport(..),
    ) where

import PlanMill.Internal.Prelude

import PlanMill.Types.Enumeration (EnumValue)
import PlanMill.Types.Identifier  (HasIdentifier (..), Identifier)
import PlanMill.Types.Project     (ProjectId)
import PlanMill.Types.Task        (TaskId)
import PlanMill.Types.UOffset     (UOffset (..))
import PlanMill.Types.User        (UserId)

type TimereportId = Identifier Timereport
type Timereports = Vector Timereport

data Timereport = Timereport
    { _trId             :: !TimereportId
    , trTask            :: !TaskId
    , trAmount          :: !(NDT 'Minutes Int)
    , trBillableStatus  :: !(EnumValue Timereport "billableStatus")
    , trBillingComment  :: !(Maybe Text)
    , trComment         :: !(Maybe Text)
    , trDutyType        :: !(Maybe (EnumValue Timereport "dutyType"))
    , trFinish          :: !Day
    , trOvertimeAmount  :: !(Maybe Int)
    , trOvertimeComment :: !(Maybe Text)
    , trPerson          :: !UserId
    , trProject         :: !(Maybe ProjectId)
    , trStart           :: !Day   -- ^ The ''UTCTime' would be more precise, but we care about day more
    , trStatus          :: !(EnumValue Timereport "status") -- Note: metadata is wrong for this field. (2018-09-25)
    , trTravelAmount    :: !(Maybe Int)
    , trTravelComment   :: !(Maybe Text)
    }
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

makeLenses ''Timereport
deriveGeneric ''Timereport

instance HasIdentifier Timereport Timereport where
    identifier = trId

instance Hashable Timereport
instance NFData Timereport
instance AnsiPretty Timereport
instance Binary Timereport
instance HasStructuralInfo Timereport where structuralInfo = sopStructuralInfo
instance HasSemanticVersion Timereport

instance FromJSON Timereport where
    parseJSON = withObject "Timereport" $ \obj -> Timereport
        <$> obj .: "id"
        <*> obj .: "task"
        <*> obj .:? "amount" .!= 0  -- sometimes amount can be Null :/
        <*> obj .: "billableStatus"
        <*> obj .:? "billingComment"
        <*> (getParsedAsText <$$> obj .:? "comment")
        <*> obj .:? "dutyType"
        <*> (dayFromZ <$> obj .: "finish")
        <*> obj .: "overtimeAmount"
        <*> obj .: "overtimeComment"
        <*> obj .: "person"
        <*> obj .: "project"
        <*> (dayFromZ <$> obj .: "start")
        <*> obj .: "status"
        <*> obj .: "travelAmount"
        <*> obj .: "travelComment"

-- | Type used to create new timereports
data NewTimereport = NewTimereport
    { ntrTask     :: !TaskId
    -- , ntrProject  :: !ProjectId
    , ntrStart    :: !Day
    , ntrAmount   :: !(NDT 'Minutes Int)
    , ntrComment  :: !Text
    , ntrUser     :: !UserId
    -- , ntrDutyType :: !Int
    -- , ntrStatus   :: !Int
    }
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

deriveGeneric ''NewTimereport

instance Hashable NewTimereport
instance NFData NewTimereport
instance AnsiPretty NewTimereport
instance Binary NewTimereport
instance HasStructuralInfo NewTimereport where structuralInfo = sopStructuralInfo
instance HasSemanticVersion NewTimereport

instance ToJSON NewTimereport where
    toJSON NewTimereport {..} = object
        [ "task"    .= ntrTask
        -- , "project" .= ntrProject
        , "start"   .= UOffset (UTCTime ntrStart 0)
        , "amount"  .= ntrAmount
        , "comment" .= ntrComment
        , "person"  .= ntrUser
        -- , "dutyType" .= ntrDutyType
        -- , "status" .= ntrStatus
        ]

-- | Type used to create new timereports
data EditTimereport = EditTimereport
    { _etrId      :: !TimereportId
    , etrTask     :: !TaskId
    -- , etrProject  :: !ProjectId
    , etrStart    :: !Day
    , etrAmount   :: !(NDT 'Minutes Int)
    , etrComment  :: !Text
    , etrUser     :: !UserId
    -- , etrDutyType :: !Int
    -- , etrStatus   :: !Int
    }
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

makeLenses ''EditTimereport
deriveGeneric ''EditTimereport

instance Hashable EditTimereport
instance NFData EditTimereport
instance AnsiPretty EditTimereport
instance Binary EditTimereport
instance HasStructuralInfo EditTimereport where structuralInfo = sopStructuralInfo
instance HasSemanticVersion EditTimereport

instance ToJSON EditTimereport where
    toJSON EditTimereport {..} = object
        [ "id"      .= _etrId
        , "task"    .= etrTask
        -- , "project" .= etrProject
        , "start"   .= UOffset (UTCTime etrStart 0)
        , "amount"  .= etrAmount
        , "comment" .= etrComment
        , "person"  .= etrUser
        -- , "dutyType" .= etrDutyType
        -- , "status" .= etrStatus
        ]

instance HasIdentifier EditTimereport Timereport where
    identifier = etrId
