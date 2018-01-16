{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- |
-- HoursRemaining: Task.taskTargetEffort - (totalHoursUsedByAssignedPersonnel)
--
-- /NOTE:/ Golang backend LatestEntry is Entry with a Date
module Futurice.App.HoursApi.Types where

import Control.Lens              (foldOf, imap, sumOf)
import Data.Aeson                (Value (..), withText)
import Data.Aeson.Types          (typeMismatch)
import Data.Fixed                (Centi)
import Data.Swagger              (NamedSchema (..))
import Futurice.Generics
import Futurice.Monoid           (Average (..))
import Futurice.Prelude
import Futurice.Time
import Futurice.Time.Month       (dayToMonth)
import Numeric.Interval.NonEmpty (Interval, inf, sup)
import Prelude ()
import Test.QuickCheck           (arbitraryBoundedEnum)

import qualified Data.Map        as Map
import qualified PlanMill        as PM
import qualified Test.QuickCheck as QC

import Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import Database.PostgreSQL.Simple.ToRow   (ToRow (..))

-------------------------------------------------------------------------------
-- Project
-------------------------------------------------------------------------------

data Project task = Project
    { _projectId     :: PM.ProjectId
    , _projectName   :: !Text
    , _projectTasks  :: [task]
    , _projectClosed :: !Bool
    }
  deriving (Eq, Show, Typeable, Generic)

data ReportableTask = ReportableTask
    { _rtaskId             :: PM.TaskId
    , _rtaskName           :: !Text
    , _rtaskClosed         :: !Bool
    , _rtaskLatestEntry    :: !(Maybe LatestEntry)
    , _rtaskHoursRemaining :: !(Maybe (NDT 'Hours Centi))
    }
  deriving (Eq, Show, Typeable, Generic)

mkTask :: PM.TaskId -> Text -> ReportableTask
mkTask i name = ReportableTask
    { _rtaskId             = i
    , _rtaskName           = name
    , _rtaskClosed         = False
    , _rtaskLatestEntry    = Nothing
    , _rtaskHoursRemaining = Nothing
    }

data MarkedTask = MarkedTask
    { _mtaskId     :: PM.TaskId
    , _mtaskName   :: !Text
    , _mtaskClosed :: !Bool
    , _mtaskAbsence :: !Bool
    }
  deriving (Eq, Show, Typeable, Generic)

-- | Entry may be billable, not billable, or not-countable (i.e. absences)
data EntryType
    = EntryTypeBillable
    | EntryTypeNotBillable
    | EntryTypeAbsence
  deriving (Eq, Ord, Show, Enum, Bounded, Typeable, Generic)

-- Entries for a specific Day
data Entry = Entry
    { _entryId          :: !PM.TimereportId
    , _entryProjectId   :: !PM.ProjectId
    , _entryTaskId      :: !PM.TaskId
    , _entryDay         :: !Day
    , _entryDescription :: !Text
    , _entryClosed      :: !Bool
    , _entryHours       :: !(NDT 'Hours Centi)
    , _entryBillable    :: !EntryType
    }
  deriving (Eq, Show, Typeable, Generic)

entryUtilizationAvg :: Getter Entry (Maybe (Average Float))
entryUtilizationAvg = getter $ \entry ->
    let NDT hours = _entryHours entry in case _entryBillable entry of
        EntryTypeBillable    -> Just $ Average (realToFrac hours) 100
        EntryTypeNotBillable -> Just $ Average (realToFrac hours) 0
        EntryTypeAbsence     -> Nothing

-- TODO: perhaps a lens getter for an Entry?

-- | Hours UI feature. Previous Entry used as default values when marking new hours.
data LatestEntry = LatestEntry
    { _latestEntryDescription :: !Text
    , _latestEntryDate        :: !Day
    , _latestEntryHours       :: !(NDT 'Hours Centi)
    }
  deriving (Eq, Show, Typeable, Generic)

-- | When frontend sends closed entry to be updated, API doesn't do anything, just respond ok
--
-- /TODO:/ is this *new* entry, probably we don't need closed field at all?
data EntryUpdate = EntryUpdate
    { _euTaskId      :: PM.TaskId
    , _euProjectId   :: PM.ProjectId
    , _euDescription :: !(Maybe Text)
    , _euDate        :: !Day
    , _euHours       :: !(NDT 'Hours Centi)
    , _euClosed      :: !(Maybe Bool)
    }
  deriving (Eq, Show, Typeable, Generic)

data EntryUpdateResponse = EntryUpdateResponse
    { _eurUser  :: !User
    , _eurHours :: !HoursResponse
    }
  deriving (Eq, Show, Typeable, Generic)

-- | Golang: UserResponse
data User = User
    { _userFirstName       :: !Text
    , _userLastName        :: !Text
    , _userBalance         :: !(NDT 'Hours Centi)
    , _userHolidaysLeft    :: !(NDT 'Days Centi)
    , _userUtilizationRate :: !Float
    , _userProfilePicture  :: !Text
    }
  deriving (Eq, Show, Typeable, Generic)

-- |
data HoursDay = HoursDay
    { _dayType    :: !DayType
    , _dayHours   :: !(NDT 'Hours Centi)
    , _dayEntries :: ![Entry]
    , _dayClosed  :: !Bool
    }
  deriving (Eq, Show, Typeable, Generic)

data DayType
    = DayTypeNormal
    | DayTypeZero           -- ^ zero capacity, weekend or other non-working day
    | DayTypeHoliday !Text  -- ^ named holiday
  deriving (Eq, Show, Typeable, Generic)

defaultHoursDay :: HoursDay
defaultHoursDay = HoursDay
    { _dayType    = DayTypeNormal
    , _dayHours   = 0
    , _dayEntries = []
    , _dayClosed  = False
    }

-- | TODO: add a '_samples' of Utilisation rate weighted average.
data HoursMonth = HoursMonth
    { _monthHours           :: !(NDT 'Hours Centi)
    , _monthUtilizationRate :: !Float
    , _monthDays            :: Map Day HoursDay -- ^ invariant days of the same month
    }
  deriving (Eq, Show, Typeable, Generic)

data HoursResponse = HoursResponse
    { _hoursResponseDefaultWorkHours    :: !(NDT 'Hours Centi)
    , _hoursResponseReportableProjects  :: ![Project ReportableTask]
    , _hoursResponseMarkedProjects      :: ![Project MarkedTask]
    , _hoursResponseMonths              :: Map Month HoursMonth -- invariant contents: 'HoursMonth' contains days of key-month
    }
  deriving (Eq, Show, Typeable, Generic)

data SettingsResponse = SettingsResponse
    { _settingsResponseWeeklyView :: !Bool
    , _settingsResponseShowGraphs :: !Bool
    }
  deriving (Eq, Show, Typeable, Generic)

data SettingsUpdateResponse = SettingsUpdateResponse
    { _settingsUpdateStatus :: !Text
    , _settingsUpdateUnused :: !()
    }
  deriving (Show)

-------------------------------------------------------------------------------
-- Generics and Lenses
-------------------------------------------------------------------------------

makeLenses ''Project
deriveGeneric ''Project

makeLenses ''ReportableTask
deriveGeneric ''ReportableTask

makeLenses ''MarkedTask
deriveGeneric ''MarkedTask

makeLenses ''LatestEntry
deriveGeneric ''LatestEntry

makeLenses ''Entry
deriveGeneric ''Entry

makeLenses ''EntryUpdate
deriveGeneric ''EntryUpdate

makeLenses ''EntryUpdateResponse
deriveGeneric ''EntryUpdateResponse

makeLenses ''User
deriveGeneric ''User

makeLenses ''HoursDay
deriveGeneric ''HoursDay

makePrisms ''DayType

makeLenses ''HoursMonth
deriveGeneric ''HoursMonth

makeLenses ''HoursResponse
deriveGeneric ''HoursResponse

makeLenses ''SettingsResponse
deriveGeneric ''SettingsResponse

deriveGeneric ''SettingsUpdateResponse

-------------------------------------------------------------------------------
-- Smart constructors
-------------------------------------------------------------------------------

-- here because we want to use lenses.

-- | Smart constructor.
mkHoursMonth
    :: Interval Day     -- ^ Interval to include entries from
    -> Map Day DayType  -- ^ Holiday names
    -> [Entry]
    -> Map Month HoursMonth
mkHoursMonth interval holidays entries =
    let entriesByDay :: Map Day [Entry]
        entriesByDay = Map.fromListWith (++)
            [ (_entryDay e, [e]) | e <- entries ]

        -- a map spawning whole interval
        entriesByDay' :: Map Day [a]
        entriesByDay' = Map.fromList [ (d, []) | d <- [inf interval .. sup interval] ]

        entriesByMonth :: Map Month (Map Day [Entry])
        entriesByMonth = Map.fromListWith (Map.unionWith (++))
            [ (dayToMonth d, Map.singleton d es)
            | (d, es) <- Map.toList (entriesByDay <> entriesByDay')
            ]

    in mkHoursMonth' <$> entriesByMonth
  where
    mkHoursMonth' :: Map Day [Entry] -> HoursMonth
    mkHoursMonth' mm = HoursMonth
        { _monthHours           = sumOf (folded . folded . entryHours) mm
        , _monthUtilizationRate = utz
        , _monthDays            = imap mkHoursDay mm
        }
      where
        Average _hours utz = foldOf
            (folded . folded . entryUtilizationAvg . folded)
            mm

    -- Invariant: all entries are on the first day
    mkHoursDay :: Day -> [Entry] -> HoursDay
    mkHoursDay d es = HoursDay
        { _dayType    = fromMaybe DayTypeNormal $ holidays ^? ix d
        , _dayHours   = sumOf (folded . entryHours) es
        , _dayEntries = es
        , _dayClosed  = False -- TODO: not correct: andOf (folded . entryClosed) es
          -- day is closed if every entry in it is closed
        }

latestEntryFromEntry :: Entry -> LatestEntry
latestEntryFromEntry e = LatestEntry
    { _latestEntryDescription = e ^. entryDescription
    , _latestEntryDate        = e ^. entryDay
    , _latestEntryHours       = e ^. entryHours
    }

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Arbitrary EntryType where
    arbitrary = arbitraryBoundedEnum
    shrink EntryTypeBillable = []
    shrink et                = [ EntryTypeBillable .. pred et ]

entryTypeText :: EntryType -> Text
entryTypeText EntryTypeBillable    = "billable"
entryTypeText EntryTypeNotBillable = "non-billable"
entryTypeText EntryTypeAbsence     = "absence"

instance ToJSON EntryType where
    toJSON = String . entryTypeText

instance FromJSON EntryType where
    parseJSON = withText "EntryType" $ \t -> maybe
        (fail $ "Invalid entry type: " <> t ^. unpacked)
        pure
        (lookup t m)
      where
        m = [ (entryTypeText et, et) | et <- [ minBound .. maxBound ] ]

instance ToSchema EntryType where
    declareNamedSchema _ = pure $ NamedSchema (Just "EntryType") mempty

instance Arbitrary task => Arbitrary (Project task) where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON task => ToJSON (Project task) where
    toJSON = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON task => FromJSON (Project task) where parseJSON = sopParseJSON
instance ToSchema task => ToSchema (Project task) where declareNamedSchema = sopDeclareNamedSchema

instance Arbitrary ReportableTask where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON ReportableTask where
    toJSON = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON ReportableTask where parseJSON = sopParseJSON
instance ToSchema ReportableTask where declareNamedSchema = sopDeclareNamedSchema

instance Arbitrary MarkedTask where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON MarkedTask where
    toJSON = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON MarkedTask where parseJSON = sopParseJSON
instance ToSchema MarkedTask where declareNamedSchema = sopDeclareNamedSchema

instance Arbitrary LatestEntry where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON LatestEntry where
    toJSON = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON LatestEntry where parseJSON = sopParseJSON
instance ToSchema LatestEntry where declareNamedSchema = sopDeclareNamedSchema

instance Arbitrary Entry where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON Entry where
    toJSON = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON Entry where parseJSON = sopParseJSON
instance ToSchema Entry where declareNamedSchema = sopDeclareNamedSchema

instance Arbitrary EntryUpdate where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON EntryUpdate where
    toJSON = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON EntryUpdate where parseJSON = sopParseJSON
instance ToSchema EntryUpdate where declareNamedSchema = sopDeclareNamedSchema

instance Arbitrary EntryUpdateResponse where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON EntryUpdateResponse where
    toJSON = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON EntryUpdateResponse where parseJSON = sopParseJSON
instance ToSchema EntryUpdateResponse where declareNamedSchema = sopDeclareNamedSchema

instance Arbitrary User where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON User where
    toJSON = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON User where parseJSON = sopParseJSON
instance ToSchema User where declareNamedSchema = sopDeclareNamedSchema

instance Arbitrary DayType where
    arbitrary = QC.elements [ DayTypeNormal, DayTypeZero, DayTypeHoliday "Christmas" ]

-- | Truthy value indicates it's a holiday!
instance ToJSON DayType where
    toJSON DayTypeNormal      = Bool False
    toJSON DayTypeZero        = Bool True
    toJSON (DayTypeHoliday n) = String n

instance FromJSON DayType where
    parseJSON (Bool False) = pure DayTypeNormal
    parseJSON (Bool True)  = pure DayTypeZero
    parseJSON (String n)   = pure (DayTypeHoliday n)
    parseJSON v            = typeMismatch "DayType" v

-- | TODO
instance ToSchema DayType where
    declareNamedSchema _ = pure $ NamedSchema (Just "Day type") mempty

instance Arbitrary HoursDay where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON HoursDay where
    toJSON = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON HoursDay where parseJSON = sopParseJSON
instance ToSchema HoursDay where declareNamedSchema = sopDeclareNamedSchema

instance Arbitrary HoursMonth where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON HoursMonth where
    toJSON = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON HoursMonth where parseJSON = sopParseJSON
instance ToSchema HoursMonth where declareNamedSchema = sopDeclareNamedSchema

instance Arbitrary HoursResponse where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToJSON HoursResponse where
    toJSON = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON HoursResponse where parseJSON = sopParseJSON
instance ToSchema HoursResponse where declareNamedSchema = sopDeclareNamedSchema

instance ToJSON SettingsResponse where
    toJSON = sopToJSON
    toEncoding = sopToEncoding
instance FromJSON SettingsResponse where parseJSON = sopParseJSON
instance ToSchema SettingsResponse where declareNamedSchema = sopDeclareNamedSchema
instance FromRow SettingsResponse where
  fromRow = SettingsResponse <$> field <*> field
instance ToRow SettingsResponse where
  toRow (SettingsResponse a b) = toRow (a,b)

instance ToJSON SettingsUpdateResponse where
  toJSON = sopToJSON
  toEncoding = sopToEncoding
instance ToSchema SettingsUpdateResponse where declareNamedSchema = sopDeclareNamedSchema
