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

import Data.RFC5051            (compareUnicode)
import Futurice.App.Avatar.API
import Futurice.Constants      (avatarPublicUrl)
import Futurice.Email          (emailToText)
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()
import Servant

import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T

-- Data definition
import qualified Chat.Flowdock.REST as FD
import qualified FUM.Types.Login    as FUM
import qualified GitHub             as GH
import qualified Personio

-- Contacts modules
import Futurice.App.Contacts.Types

compareUnicodeText :: Text -> Text -> Ordering
compareUnicodeText = compareUnicode `on` T.unpack

-- | Default avatar.
noImage :: Text
noImage = "https://avatars0.githubusercontent.com/u/852157?v=3&s=30"

-- | Get contacts data
contacts
    :: ( MonadFlowdock m, MonadGitHub m, Personio.MonadPersonio m
       , MonadTime m, MonadReader env m
       , HasGithubOrgName env, HasFUMEmployeeListName env, HasFlowdockOrgName env
       )
    => m [Contact Text]
contacts = contacts'
    <$> currentDay
    <*> Personio.personio Personio.PersonioEmployees
    <*> githubDetailedMembers
    <*> flowdockOrganisation

-- | The pure, data mangling part of 'contacts'
contacts'
    :: Day
    -> [Personio.Employee]
    -> Vector GH.User
    -> FD.Organisation
    -> [Contact Text]
contacts' today employees githubMembers flowdockOrg =
    let employees' = filter (Personio.employeeIsActive today) employees
        res0 = map employeeToContact employees'
        res1 = addGithubInfo githubMembers res0
        res2 = addFlowdockInfo (flowdockOrg ^. FD.orgUsers) res1
    in sortBy (compareUnicodeText `on` contactName) res2

employeeToContact :: Personio.Employee -> Contact Text
employeeToContact e = Contact
    { contactLogin      = fumLogin
    , contactFirst      = e ^. Personio.employeeFirst
    , contactName       = e ^. Personio.employeeFirst <> " " <> e ^. Personio.employeeLast
    , contactEmail      = maybe "" emailToText $ e ^. Personio.employeeEmail
    , contactPhones     = catMaybes [e ^. Personio.employeeWorkPhone, e ^. Personio.employeeHomePhone]
    , contactTitle      = e ^. Personio.employeePosition
    , contactThumb      = avatarPublicUrl <> linkToText (safeLink avatarApi fumAvatarEndpoint fumLogin thumbSize False)
    , contactImage      = avatarPublicUrl <> linkToText (safeLink avatarApi fumAvatarEndpoint fumLogin imageSize False)
    , contactFlowdock   = mcase (e ^. Personio.employeeFlowdock) Nothing $
        Just . (\uid -> ContactFD (fromIntegral $ FD.getIdentifier uid) "-" noImage)
    , contactGithub     = mcase (e ^. Personio.employeeGithub) Nothing $
        Just . flip (ContactGH . GH.untagName) noImage
    , contactTeam       = e ^. Personio.employeeTribe
    , contactOffice     = e ^. Personio.employeeOffice
    , contactEmployer   = e ^. Personio.employeeEmployer
    , contactCountry    = e ^. Personio.employeeCountry
    , contactCompetence = e ^. Personio.employeeRole
    , contactExternal   = Just Personio.External == e ^. Personio.employeeEmploymentType
    , contactHrnumber   = e ^. Personio.employeeHRNumber
    }
  where
    thumbSize = Nothing
    imageSize = Nothing
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

addFlowdockInfo
    :: forall u f g. (FD.UserLike u, Functor f, Foldable g)
    => g u
    -> f (Contact Text)
    -> f (Contact Text)
addFlowdockInfo us = fmap add
  where
    -- we could use ixsed-typed for these
    emailMap :: HM.HashMap Text u
    emailMap = foldMap (\u -> HM.singleton (u ^. FD.userEmail) u) us

    nameMap :: HM.HashMap Text u
    nameMap = foldMap (\u -> HM.singleton (u ^. FD.userName) u) us

    uidMap :: HM.HashMap FD.UserId u
    uidMap = foldMap (\u -> HM.singleton (u ^. FD.userId) u) us

    add :: Contact Text -> Contact Text
    add c = c
        { contactFlowdock =
            (cfd >>= byId . FD.mkIdentifier . fromIntegral . cfdId)
            <|> byEmail
            <|> byName
        }
      where
        cfd = contactFlowdock c
        name = contactName c
        email = contactEmail c

        byId uid = fmap f (HM.lookup uid uidMap)
        byEmail = fmap f (HM.lookup email emailMap)
        byName  = fmap f (HM.lookup name nameMap)

        f :: u -> ContactFD Text
        f u = ContactFD
            { cfdId     = fromIntegral $ FD.getIdentifier $ u ^. FD.userId
            , cfdNick   = truncateNick $ u ^. FD.userNick
            , cfdAvatar = u ^. FD.userAvatar
            }

        truncateNick :: Text -> Text
        truncateNick t
            | T.length t >= 20 = T.take 17 t <> "..."
            | otherwise        = t

linkToText :: Link -> Text
linkToText l = "/" <> toUrlPiece l
