{-# LANGUAGE CPP                   #-}
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
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
#endif
module Futurice.App.Contacts.Logic (
    contacts,
    ) where

import Data.RFC5051          (compareUnicode)
import Futurice.Integrations
import Futurice.Prelude
import Futurice.Tribe
import Prelude ()

import qualified Data.HashMap.Strict as HM
import qualified Data.Maybe.Strict   as S
import qualified Data.Text           as T
import qualified Data.Vector         as V

-- Data definition
import qualified Chat.Flowdock.REST as FD
import qualified FUM
import qualified GitHub             as GH
import qualified Personio
import qualified PlanMill           as PM
import qualified PlanMill.Queries   as PMQ

-- Contacts modules
import Futurice.App.Contacts.Types
import Futurice.App.Contacts.Types.Tri (lessSure)

compareUnicodeText :: Text -> Text -> Ordering
compareUnicodeText = compareUnicode `on` T.unpack

-- | Get contacts data
contacts
    :: ( MonadFlowdock m, MonadGitHub m, MonadFUM m, MonadPlanMillQuery m, Personio.MonadPersonio m
       , MonadTime m, MonadReader env m
       , HasGithubOrgName env, HasFUMEmployeeListName env, HasFlowdockOrgName env
       )
    => m [Contact Text]
contacts = contacts'
    <$> currentTime
    <*> Personio.personio Personio.PersonioEmployees
    <*> fumEmployeeList
    <*> githubDetailedMembers
    <*> flowdockOrganisation
    <*> fumPlanmillCompetenceMap
    <*> fumPlanmillTeamMap

-- | The pure, data mangling part of 'contacts'
contacts'
    :: UTCTime
    -> [Personio.Employee]
    -> Vector FUM.User
    -> Vector GH.User
    -> FD.Organisation
    -> HashMap FUM.Login (Maybe Text)
    -> HashMap FUM.Login (Maybe Text)
    -> [Contact Text]
contacts' now employees _users githubMembers flowdockOrg competenceMap teamMap =
    let employees' = filter (Personio.employeeIsActive now) employees
        res0 = map employeeToContact employees'
        res1 = addGithubInfo githubMembers res0
        res2 = addFlowdockInfo (flowdockOrg ^. FD.orgUsers) res1
        res3 = addPlanmillInfo competenceMap teamMap res2
    in sortBy (compareUnicodeText `on` contactName) res3

employeeToContact :: Personio.Employee -> Contact Text
employeeToContact e = Contact
    { contactLogin      = fromMaybe $(FUM.mkLogin "xxxx") $ e ^. Personio.employeeLogin
    , contactFirst      = e ^. Personio.employeeFirst
    , contactName       = e ^. Personio.employeeFirst <> " " <> e ^. Personio.employeeLast
    , contactEmail      = e ^. Personio.employeeEmail
    , contactPhones     = catMaybes [e ^. Personio.employeeWorkPhone, e ^. Personio.employeeHomePhone]
    , contactTitle      = Just "title todo"
    , contactThumb      = noImage -- from FUM
    , contactImage      = noImage -- from FUM
    , contactFlowdock   = maybe Unknown
                                (Sure . (\uid -> ContactFD (fromIntegral $ FD.getIdentifier uid) "-" noImage))
                                $ e ^. Personio.employeeFlowdock
    , contactGithub     = maybe Unknown 
                                (Sure . flip (ContactGH . ghProfileLink) noImage) 
                                $ e ^. Personio.employeeGithub
    , contactTeam       = Just (tribeToText $ e ^. Personio.employeeTribe)
    -- , contactOffice
    , contactCompetence = Just "competence todo"
    }
  where
    noImage = "https://avatars0.githubusercontent.com/u/852157?v=3&s=30"
    ghProfileLink = T.append githubPrefix . GH.untagName 
      where
        githubPrefix = "https://github.com/"

_userToContact :: FUM.User -> Contact Text
_userToContact FUM.User{..} = Contact
    { contactLogin      = _userName
    , contactFirst      = _userFirst
    , contactName       = _userFirst <> " " <> _userLast
    , contactEmail      = S.fromMaybe defaultEmail _userEmail
    , contactPhones     = S.catMaybes [_userPhone1, _userPhone2]
    , contactTitle      = _userTitle ^. lazy
    , contactThumb      = S.fromMaybe noImage _userThumbUrl
    , contactImage      = S.fromMaybe noImage _userImageUrl
    , contactFlowdock   = S.maybe Unknown (Sure . (\uid -> ContactFD uid "-" noImage)) _userFlowdock
    , contactGithub     = S.maybe Unknown (Sure . flip ContactGH noImage) _userGithub
    , contactTeam       = Nothing
    , contactCompetence = Nothing
    }
  where
    noImage = "https://avatars0.githubusercontent.com/u/852157?v=3&s=30"
    defaultEmail = FUM.loginToText _userName <> "@futurice.com"

githubDetailedMembers
    :: ( MonadGitHub m
       , MonadReader env m, HasGithubOrgName env
       )
    => m (Vector GH.User)
githubDetailedMembers = pure mempty {- do
    githubMembers <- githubOrganisationMembers
    traverse (githubReq . GH.userInfoForR . GH.simpleUserLogin) githubMembers
    -}

fumPlanmillTeamMap
    :: forall m env.
       ( MonadPlanMillQuery m, MonadFUM m
       , MonadReader env m, HasFUMEmployeeListName env
       )
    => m (HashMap FUM.Login (Maybe Text))
fumPlanmillTeamMap = fumPlanmillMap >>= traverse (f . snd)
  where
    f :: PM.User -> m (Maybe Text)
    f u = PM.tName <$$> traverse PMQ.team (PM.uTeam u)

fumPlanmillCompetenceMap
    :: forall m env.
       ( MonadPlanMillQuery m, MonadFUM m
       , MonadReader env m, HasFUMEmployeeListName env
       )
    => m (HashMap FUM.Login (Maybe Text))
fumPlanmillCompetenceMap = PM.uCompetence . snd  <$$> fumPlanmillMap

addGithubInfo
    :: (Functor f, Foldable g)
    => g GH.User -> f (Contact Text) -> f (Contact Text)
addGithubInfo gh = fmap add
  where
    gh' = toList gh

    nameMap :: HM.HashMap Text GH.User
    nameMap = HM.fromList (mapMaybe pair gh')
      where
        pair :: GH.User -> Maybe (Text, GH.User)
        pair x = (\y -> (y, x)) <$> GH.userName x

    loginMap :: HM.HashMap Text GH.User
    loginMap = HM.fromList (map pair gh')
      where
        pair :: GH.User -> (Text, GH.User)
        pair x = (GH.untagName $ GH.userLogin x, x)

    add :: Contact Text -> Contact Text
    add c = c{ contactGithub = (cgh >>= byLogin . cghNick)
                             <> lessSure cgh
                             <> byName }
      where
        cgh :: Tri (ContactGH Text)
        cgh = contactGithub c

        byName :: Tri (ContactGH Text)
        byName = maybe Unknown
                       (Unsure . fromDetailedOwner)
                       (HM.lookup (contactName c) nameMap)

        byLogin :: Text -> Tri (ContactGH Text)
        byLogin ghLogin = maybe Unknown
                                (Sure . fromDetailedOwner)
                                (HM.lookup ghLogin loginMap)

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
            <> lessSure cfd
            <> byEmail
            <> byName
        }
      where
        cfd = contactFlowdock c
        name = contactName c
        email = contactEmail c

        byId uid = maybe Unknown (Sure . f) (HM.lookup uid uidMap)
        byEmail = maybe Unknown (Sure . f)   (HM.lookup email emailMap)
        byName  = maybe Unknown (Unsure . f) (HM.lookup name nameMap)

        f :: u -> ContactFD Text
        f u = ContactFD (fromIntegral $ FD.getIdentifier $ u ^. FD.userId)
                        (u ^. FD.userNick)
                        (u ^. FD.userAvatar)

addPlanmillInfo
    :: Functor f
    => HashMap FUM.Login (Maybe Text)
    -> HashMap FUM.Login (Maybe Text)
    -> f (Contact a)
    -> f (Contact a)
addPlanmillInfo competenceMap teamMap = fmap f
  where
    f :: Contact a -> Contact a
    f c = c
        { contactTeam       = join $ HM.lookup login teamMap
        , contactCompetence = join $ HM.lookup login competenceMap
        }
      where
        login = contactLogin c
