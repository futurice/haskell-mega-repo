{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
module Futurice.App.Contacts.Logic (
    contacts,
    ) where

import Data.RFC5051          (compareUnicode)
import Futurice.Constants    (avatarPublicUrl)
import Futurice.Email        (emailToText)
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()
import Servant

import qualified Data.HashMap.Strict     as HM
import qualified Data.Map.Strict         as Map
import qualified Futurice.App.Avatar.API as Avatar

-- Data definition
import qualified FUM.Types.Login as FUM
import qualified GitHub          as GH
import qualified Personio
import qualified Power
import qualified Slack

-- Contacts modules
import Futurice.App.Contacts.Types

-- | Default avatar.
noImage :: Text
noImage = "https://avatars0.githubusercontent.com/u/852157?v=3&s=30"

-- | Get contacts data
contacts
    :: ( MonadGitHub m, Personio.MonadPersonio m
       , Power.MonadPower m
       , MonadTime m, MonadReader env m
       , HasGithubOrgName env, HasFUMEmployeeListName env
       , MonadSlack m
       )
    => m [Contact Text]
contacts = contacts'
    <$> currentDay
    <*> Personio.personio Personio.PersonioEmployees
    <*> Power.powerPeople
    <*> githubDetailedMembers
    <*> Slack.slackProfiles

-- | The pure, data mangling part of 'contacts'
contacts'
    :: Day
    -> [Personio.Employee]
    -> [Power.Person]
    -> Vector GH.User
    -> [Slack.User]
    -> [Contact Text]
contacts' today employees powPeople githubMembers slackUsers =
    let employees' = filter (Personio.employeeIsActive today) employees
        res0 = map employeeToContact employees'
        res1 = addGithubInfo githubMembers res0
        res2 = addPowerInfo powPeople res1
        res3 = addSlackInfo slackUsers res2
    in sortBy (compareUnicode `on` contactName) res3

employeeToContact :: Personio.Employee -> Contact Text
employeeToContact e = Contact
    { contactLogin      = fumLogin
    , contactFirst      = e ^. Personio.employeeFirst
    , contactName       = e ^. Personio.employeeFirst <> " " <> e ^. Personio.employeeLast
    , contactEmail      = maybe "" emailToText $ e ^. Personio.employeeEmail
    , contactPhones     = catMaybes [e ^. Personio.employeeWorkPhone] --TODO: remove list type
    , contactTitle      = e ^. Personio.employeePosition
    , contactThumb      = avatarPublicUrl <> fieldLink' linkToText Avatar.recFum fumLogin Nothing False
    , contactImage      = avatarPublicUrl <> fieldLink' linkToText Avatar.recFum fumLogin (Just Avatar.Original) False
    , contactGithub     = mcase (e ^. Personio.employeeGithub) Nothing $
        Just . flip (ContactGH . GH.untagName) noImage
    , contactTeam       = e ^. Personio.employeeTribe
    , contactOffice     = e ^. Personio.employeeOffice
    , contactEmployer   = e ^. Personio.employeeEmployer
    , contactCountry    = e ^. Personio.employeeCountry
    , contactCompetence = e ^. Personio.employeeRole
    , contactExternal   = Just Personio.External == e ^. Personio.employeeEmploymentType
    , contactHrnumber   = e ^. Personio.employeeHRNumber
    , contactPersonio   = e ^. Personio.employeeId
    , contactUtzTarget  = 0 -- to be corrected
    , contactSlack      = Nothing
    }
  where
    fumLogin = fromMaybe $(FUM.mkLogin "xxxx") $ e ^. Personio.employeeLogin

githubDetailedMembers
    :: ( MonadGitHub m
       , MonadReader env m, HasGithubOrgName env
       )
    => m (Vector GH.User)
githubDetailedMembers = do
    githubMembers <- githubOrganisationMembers
    traverse (githubReq . GH.userInfoForR . GH.simpleUserLogin) githubMembers

addGithubInfo
    :: (Functor f, Foldable g)
    => g GH.User -> f (Contact Text) -> f (Contact Text)
addGithubInfo gh = fmap add
  where
    gh' = toList gh

    loginMap :: HM.HashMap (GH.Name GH.User) GH.User
    loginMap = HM.fromList (map pair gh')
      where
        pair :: GH.User -> (GH.Name GH.User, GH.User)
        pair x = (GH.userLogin x, x)

    add :: Contact Text -> Contact Text
    add c = c
        { contactGithub = cgh >>= byLogin . cghNick
        }
      where
        cgh :: Maybe (ContactGH Text)
        cgh = contactGithub c

        byLogin :: Text -> Maybe (ContactGH Text)
        byLogin ghLogin = fromDetailedOwner <$> HM.lookup (GH.mkUserName ghLogin) loginMap

fromDetailedOwner :: GH.User -> ContactGH Text
fromDetailedOwner gh = ContactGH
    { cghNick   = GH.untagName . GH.userLogin $ gh
    , cghAvatar = GH.getUrl $ GH.userAvatarUrl gh
    }

linkToText :: Link -> Text
linkToText l = "/" <> toUrlPiece l

addPowerInfo :: [Power.Person] -> [Contact Text] -> [Contact Text]
addPowerInfo powerPeople = map add
  where
    fumMap :: Map FUM.Login Power.Person
    fumMap = Map.fromList [ ( Power.personLogin p, p) | p <- powerPeople ]

    add c = c
        { contactUtzTarget = fromMaybe 0 $
            fumMap ^? ix (contactLogin c) . getter Power.personUtzTarget
        }

addSlackInfo :: [Slack.User] -> [Contact Text] -> [Contact Text]
addSlackInfo slackUsers = map add
  where
    slackUsersMap = Map.fromList $ (\u -> (Slack.slackName u <> "@futurice.com", u)) <$> slackUsers

    emailToSlack :: Text -> Maybe (ContactSlack Text)
    emailToSlack e = ContactSlack
        <$> (e `Map.lookup` slackUsersMap >>= pure . Slack.slackDisplayName)
        <*> (e `Map.lookup` slackUsersMap >>= pure . Slack.slackImageUrl)

    add c = c
        { contactSlack = emailToSlack (contactEmail c)
        }
