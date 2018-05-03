{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- This module exist to break cycles.
-- There are mutually referencing types: 'User' and 'Team'.
module PlanMill.Types.User (
    -- * User
    User(..),
    UserId,
    Users,
    userLogin,
    userHref_,
    -- * Team
    Team(..),
    TeamId,
    Teams,
    ) where

import Data.Aeson.Extra.SymTag         (SymTag)
import Futurice.Constants              (planmillPublicUrl)
import Lucid                           (Attribute, a_, class_, href_, toHtml)
import PlanMill.Internal.Prelude
import PlanMill.Types.Account          (AccountId)
import PlanMill.Types.CapacityCalendar (CapacityCalendarId)
import PlanMill.Types.Enumeration      (EnumValue)
import PlanMill.Types.Identifier
       (HasIdentifier (..), Identifier (..), IdentifierToHtml (..))
import Text.Regex.Applicative.Text     (match)

import qualified FUM.Types.Login as FUM

import PlanMill.Types.UOffset          (UOffset (..))

instance IdentifierToHtml User where
    identifierToHtml uid@(Ident i) = a_ attrs (toHtml t)
      where
        t = textShow i
        attrs =
            [ class_ "planmill"
            , userHref_ uid
            ]

userHref_ :: UserId -> Attribute
userHref_  (Ident i) = href_ $
    planmillPublicUrl <> "?category=Employee directory.Contact.Single contact.Summary&Id=" <> t
  where
    t = textShow i

data User = User
    { _uId               :: !UserId
    , uFirstName         :: !Text
    , uLastName          :: !Text
    , uAccount           :: !(Maybe AccountId)
    -- , uAccountName       :: !(Maybe Text)
    , uBalanceAdjustment :: !(Maybe Int)
    , uBalanceMaximum    :: !(Maybe Int)
    , uCalendars         :: !(Maybe CapacityCalendarId)
    -- TODO: users returns Text, users/:id returns Int
    -- https://github.com/planmill/api/issues/11
    , uCompetence        :: !(Maybe Text)
    , uContractType      :: !(EnumValue User "contractType")
    , uCurrencyCode      :: !(Maybe Text)
    , uDepartDate        :: !(Maybe Day)
    , uDepartment        :: !(Maybe Text)
    , uEffortUnit        :: !(Maybe Int)
    , uEmail             :: !(Maybe Text)
    , uHireDate          :: !(Maybe Day)
    , uLanguageCode      :: !(Maybe Text)
    -- , uLastLogin         :: !(Maybe UTCTime)
    -- , uLastLogout        :: !(Maybe UTCTime)
    , uPassive           :: !(EnumValue User "passive")
    , uPhoto             :: !(Maybe Text)
    , uPrimaryPhone      :: !(Maybe Text)
    , uRole              :: !(Maybe Int) -- TODO: RoleId
    , uRoleName          :: !(Maybe Text)
    -- , uSkin              :: !(Maybe Int)
    , uSuperior          :: !(Maybe UserId)
    , uTeam              :: !(Maybe TeamId)
    -- , uTeamName          :: !(Maybe Text)
    -- , uTeams             :: !(Maybe Int)
    -- , uTimeZone          :: !(Maybe Text) -- can be 0 or Text
    , uTitle             :: !(Maybe Text)
    , uUserName          :: !Text
    , uOperationalId     :: !(Maybe Int)
    }
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

userLogin :: User -> Maybe FUM.Login
userLogin = match loginRe . uUserName where
    loginRe = "https://login.futurice.com/openid/" *> FUM.loginRegexp

instance Hashable User
instance NFData User
instance AnsiPretty User
instance Binary User
instance HasStructuralInfo User where structuralInfo = sopStructuralInfo
instance HasSemanticVersion User

-- | This instance is very small, serialising only the fields we want ever to update.
--
-- /TODO:/ make EditUser type?
instance ToJSON User where
    toJSON u = object $
        [ "id"            .= (u ^. identifier)
        , "firstName"     .= uFirstName u
        , "lastName"      .= uLastName u
        , "passive"       .= uPassive u -- status
        , "contractType"  .= uContractType u
        , "operationalId" .= normalizeOperationalId (uOperationalId u)
        ]
        ++ [ "hireDate"   .= UOffset (UTCTime x 0) | Just x <- [uHireDate u] ]
        ++ [ "departDate" .= UOffset (UTCTime x 0) | Just x <- [uDepartDate u] ]
      where
        normalizeOperationalId Nothing  = Null
        normalizeOperationalId (Just 0) = Null
        normalizeOperationalId (Just n) = toJSON n

instance FromJSON User where
    parseJSON = withObject "User" $ \obj -> User <$> obj .: "id"
        <*> obj .: "firstName"
        <*> obj .: "lastName"
        <*> obj .:? "account"
        -- <*> obj .:? "accountName"
        <*> obj .:? "balanceAdjustment"
        <*> obj .:? "balanceMaximum"
        <*> optional (getParsedAsIntegral <$> obj .: "calendars") -- TODO
        <*> (obj .:? "competence" <|> pure Nothing)
        <*> obj .: "contractType"
        <*> obj .:? "currencyCode"
        <*> (dayFromZ <$$> obj .:? "departDate" <|> emptyString <$> obj .: "departDate")
        <*> obj .:? "department"
        <*> obj .:? "effortUnit"
        <*> obj .:? "email"
        <*> (dayFromZ <$$> obj .:? "hireDate" <|> emptyString <$> obj .: "hireDate")
        <*> obj .:? "languageCode"
        -- <*> (getU <$$> obj .:? "lastLogin")
        -- <*> (getU <$$> obj .:? "lastLogout")
        <*> obj .: "passive"
        <*> obj .:? "photo"
        <*> obj .:? "primaryPhone"
        <*> obj .:? "role"
        <*> obj .:? "roleName"
        -- <*> obj .:? "skin"
        <*> obj .:? "superior"
        <*> (obj .:? "team" <|> emptyString <$> obj .: "team")
        -- <*> obj .:? "teamName"
        -- <*> obj .:? "teams"
        -- <*> obj .:? "timeZone"
        <*> obj .:? "title"
        <*> obj .: "userName"
        <*> optional (getParsedAsIntegral <$> obj .: "operationalId")
      where
        -- | TODO: remove me, I'm a hack
        emptyString :: SymTag "" -> Maybe a
        emptyString _ = Nothing

data Team = Team
    { _tId            :: !TeamId
    , tName           :: !Text
    , tChildTeams     :: !(Maybe Int)
    , tCostCenter     :: !(Maybe Int)
    , tCreated        :: !(Maybe UTCTime)
    , tCreatedBy      :: !(Maybe UserId)
    , tDescription    :: !(Maybe Text)
    , tLatestMember   :: !(Maybe Int)
    , tManager        :: !(Maybe UserId)
    , tMembers        :: !(Maybe Int)
    , tModified       :: !(Maybe UTCTime)
    , tModifiedBy     :: !(Maybe UserId)
    , tParentTeam     :: !(Maybe TeamId)
    , tParentTeamName :: !(Maybe Text)
    , tPassive        :: !(Maybe Int)
    , tOwner          :: !(Maybe UserId)
    }
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

instance Hashable Team
instance NFData Team
instance AnsiPretty Team
instance Binary Team
instance HasStructuralInfo Team where structuralInfo = sopStructuralInfo
instance HasSemanticVersion Team

instance FromJSON Team where
    parseJSON = withObject "Team" $ \obj -> Team <$> obj .: "id"
        <*> obj .: "name"
        <*> obj .:? "childTeams"
        <*> obj .: "costCenter"
        <*> (getU <$$> obj .:? "created")
        <*> obj .:? "createdBy"
        <*> obj .: "description"
        <*> obj .:? "latestMember"
        <*> obj .:? "manager"
        <*> obj .:? "members"
        <*> (getU <$$> obj .:? "modified")
        <*> obj .:? "modifiedBy"
        <*> obj .:? "parentTeam"
        <*> obj .:? "parentTeamName"
        <*> obj .:? "passive"
        <*> obj .:? "owner"

type UserId = Identifier User
type Users = Vector User
type TeamId = Identifier Team
type Teams = Vector Team

makeLenses ''User
makeLenses ''Team
deriveGeneric ''User
deriveGeneric ''Team

instance HasKey User where
    type Key User = UserId
    key = uId

instance HasKey Team where
    type Key Team = TeamId
    key = tId

instance HasIdentifier User User where
    identifier = uId

instance HasIdentifier Team Team where
    identifier = tId
