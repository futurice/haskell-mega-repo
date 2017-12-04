{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.PlanMillSync.Types where

import Futurice.Prelude
import Prelude ()

import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ

data PMUser = PMUser
    { pmUser     :: !PM.User
    , pmTeam     :: !(Maybe PM.Team)
    , pmContract :: !Text
    , pmAccount  :: !(Maybe PM.Account)
    , pmPassive  :: !Text
    }
  deriving (Show, Generic)

instance NFData PMUser

users :: PMQ.MonadPlanMillQuery m => m [PMUser]
users = do
    us <- toList <$> PMQ.users
    for us $ \u -> do
        u' <- PMQ.user (u ^. PM.identifier)
        t <- traverse PMQ.team (PM.uTeam u')
        a <- traverse PMQ.account (PM.uAccount u')
        c <- PMQ.enumerationValue (PM.uContractType u) "Unknown contract"
        p <- PMQ.enumerationValue (PM.uPassive u) "Unknown active status"
        pure PMUser
            { pmUser     = u'
                { PM.uCompetence = PM.uCompetence u <|> PM.uCompetence u'
                }
            , pmTeam     = t
            , pmContract = c
            , pmAccount  = a
            , pmPassive  = p
            }
