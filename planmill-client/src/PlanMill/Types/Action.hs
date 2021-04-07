{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.Action (
    Action(..),
    Actions,
    ActionId,
    ) where

import PlanMill.Internal.Prelude

import PlanMill.Types.Contact    (ContactId)
import PlanMill.Types.Identifier (HasIdentifier (..), Identifier)
import PlanMill.Types.Project    (ProjectId)

type ActionId = Identifier Action
type Actions = Vector Action

data Action = Action
    { actionRequest     :: !(Maybe Int)
    , actionPrivate     :: !(Maybe Int)
    , actionReminder    :: !(Maybe Int)
    , actionSubject     :: !(Maybe Text)
    , actionCreated     :: !UTCTime
    , actionDueDate     :: !(Maybe UTCTime)
    , actionStart       :: !(Maybe UTCTime)
    , actionOpportunity :: !(Maybe Int)
    , actionProject     :: !(Maybe ProjectId)
    , actionType        :: !(Maybe Int)
    , actionPriority    :: !(Maybe Int)
    , actionSalesOrder  :: !(Maybe Int)
    , actionContact     :: !(Maybe ContactId)
    , actionResponsible :: !(Maybe Int)
    , actionModified    :: !(Maybe UTCTime)
    , actionCampaign    :: !(Maybe Int)
    , actionLocation    :: !(Maybe Text)
    , _actionId         :: !ActionId
    , actionStatus      :: !(Maybe Int)
    }
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

makeLenses ''Action
deriveGeneric ''Action

instance HasIdentifier Action Action where
    identifier = actionId

instance Hashable Action
instance NFData Action
instance AnsiPretty Action
instance Binary Action
instance Structured Action

instance FromJSON Action where
    parseJSON = withObject "Action" $ \obj ->
        Action <$> obj .: "request"
               <*> obj .: "private"
               <*> obj .: "reminder"
               <*> obj .: "subject"
               <*> obj .: "created"
               <*> optional (obj .: "dueDate")
               <*> optional (obj .: "start")
               <*> obj .: "opportunity"
               <*> obj .: "project"
               <*> obj .: "type"
               <*> obj .: "priority"
               <*> obj .: "salesOrder"
               <*> obj .: "contact"
               <*> obj .: "responsible"
               <*> optional (obj .: "modified")
               <*> obj .: "campaign"
               <*> obj .: "location"
               <*> obj .: "id"
               <*> obj .: "status"
