{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.Absence (
    -- * Types
    Absence(..),
    Absences,
    AbsenceId,
    -- * Functions
    absenceInterval,
    ) where

import PlanMill.Internal.Prelude
import PlanMill.Types.Enumeration (EnumValue)
import PlanMill.Types.Identifier  (HasIdentifier (..), Identifier)
import PlanMill.Types.Project     (ProjectId)
import PlanMill.Types.User        (UserId)

type AbsenceId = Identifier Absence
type Absences = Vector Absence

data Absence = Absence
    { _absenceId         :: !AbsenceId
    , absencePerson      :: !UserId
    , absenceProject     :: !ProjectId
    , absenceStart       :: !Day
    , absenceFinish      :: !Day
    , absenceAbsenceType :: !(EnumValue Absence "absenceType")
    , absenceCreated     :: !UTCTime
    {-
      2016-10-26, phadej:
      We don't actually use those fields, so let's not parse or
      store them

      Also, it might be good to never parse `description` field.

    , absenceAccepterPerson   :: !(Maybe UserId)
    , absenceInterruptionDate :: !(Maybe UTCTime)
    , absenceDescription      :: !(Maybe Text)
    , absenceVacationYear     :: !(Maybe Int)
    , absenceVacationLength   :: !Int
    , absenceModified         :: !(Maybe UTCTime)
    , absenceSubstitutePerson :: !(Maybe UserId)
    , absenceStatus           :: !Int  -- TODO
    -}
    }
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

makeLenses ''Absence
deriveGeneric ''Absence

instance Hashable Absence
instance NFData Absence
instance AnsiPretty Absence
instance Binary Absence
instance HasStructuralInfo Absence where structuralInfo = sopStructuralInfo
instance HasSemanticVersion Absence

instance HasIdentifier Absence Absence where
    identifier = absenceId

instance FromJSON Absence where
    parseJSON = withObject "Absence" $ \obj -> Absence
        <$> obj .: "id"
        <*> obj .: "person"
        <*> obj .: "project"
        <*> (dayFromZ <$> obj .: "start")
        <*> (dayFromZ <$> obj .: "finish")
        <*> obj .: "absenceType"
        <*> obj .: "created"
        {-
        <*> obj .: "accepterPerson"
        -- TODO: I'd add a combinator to aeson-extra to make this prettier
        <*> (getU <$$> obj .:? "interruptionDate")
        <*> (getU <$$> obj .:? "created")
        <*> obj .: "description"
        <*> obj .: "vacationYear"
        <*> obj .: "vacationLength"
        <*> (getU <$$> obj .:? "modified")
        <*> obj .: "substitutePerson"
        <*> obj .: "status"
        -}

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

absenceInterval :: Absence -> Interval Day
absenceInterval ab = absenceStart ab ... absenceFinish ab
