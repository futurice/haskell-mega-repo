{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Okta.Types where

import Data.Aeson        (object, (.=))
import Futurice.Company  (Country, countryToText')
import Futurice.Email
import Futurice.Generics
import Futurice.Office   (Office, officeToText)
import Futurice.Prelude
import Futurice.Tribe    (Tribe, tribeToText)
import Prelude ()

import qualified FUM
import qualified Okta     as O
import qualified Personio as P

data OktaJSON = OktaJSON
    { ojExternalGroup :: !Text,
      ojInternalGroup :: !Text,
      ojGroups        :: [GroupInfo]
    } deriving (SopGeneric, GhcGeneric, HasDatatypeInfo, Lift)
      deriving (FromJSON) via Sopica OktaJSON

data GroupInfo = GroupInfo
    { giId   :: !O.OktaGroupId
    , giName :: !Text
    } deriving (SopGeneric, GhcGeneric, HasDatatypeInfo, Lift)
      deriving (FromJSON) via Sopica GroupInfo

data UpdateInformation = UpdateInformation
    { uiOktaId           :: !O.OktaId
    , uiSecondEmail      :: !Text
    , uiEmployeeNumber   :: !(Maybe P.EmployeeId)
    , uiTribe            :: !(Maybe Tribe)
    , uiOffice           :: !(Maybe Office)
    , uiEmploymentType   :: !(Maybe P.EmploymentType)
    , uiGender           :: !(Maybe Text)
    , uiCountry          :: !(Maybe Country)
    , uiRole             :: !(Maybe Text)
    , uiStartDate        :: !(Maybe Day)
    , uiManager          :: !(Maybe Email)
    , uiFumUsername      :: !(Maybe FUM.Login)
    , uiTerminationDate  :: !(Maybe Day)
    , uiSeparationReason :: !(Maybe Text)
    , uiBirthday         :: !(Maybe Day)
    , uiCompetenceHome   :: !(Maybe Text)
    , uiMatrixSupervisor :: !(Maybe Email)
    , uiMobilePhone      :: !(Maybe Text)
    , uiClientAccount    :: !(Maybe Text)
    , uiCareerLevel      :: !(Maybe Int)
    , uiDisplayName      :: !(Maybe Text)
    , uiNationality      :: !(Maybe O.Nationality)
    } deriving (Eq, Show)

instance ToJSON UpdateInformation where
    toJSON i = object $
        [ "secondEmail"       .= uiSecondEmail i
        , "tribe"             .= (tribeToText <$> uiTribe i)
        , "office"            .= (officeToText <$> uiOffice i)
        , "employeeNumber"    .= uiEmployeeNumber i
        , "employmentType"    .= (P.employmentTypeToText <$> uiEmploymentType i)
        , "gender"            .= uiGender i
        , "country"           .= (countryToText' <$> uiCountry i)
        , "role"              .= uiRole i
        , "start_date"        .= uiStartDate i
        , "manager"           .= uiManager i
        , "fum_username"      .= uiFumUsername i
        , "terminationDate"   .= uiTerminationDate i
        , "separation_reason" .= uiSeparationReason i
        , "birthday"          .= uiBirthday i
        , "competenceHome"    .= uiCompetenceHome i
        , "matrixSupervisor"  .= uiMatrixSupervisor i
        , "mobilePhone"       .= uiMobilePhone i
        , "clientAccount"     .= uiClientAccount i
        , "careerLevel"       .= uiCareerLevel i
        , "displayName"       .= uiDisplayName i
        , "nationality"       .= uiNationality i
        ]
