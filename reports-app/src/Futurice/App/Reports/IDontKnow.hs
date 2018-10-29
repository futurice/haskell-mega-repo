{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Futurice.App.Reports.IDontKnow (
    -- * Report
    iDontKnowData,
    -- * Types
    IDontKnowData,
    IDontKnow (..),
    ) where

import Data.Fixed                (Centi)
import Data.Ord                  (comparing)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Futurice.Time
import Futurice.Integrations.TimereportKind
       (TimereportKind (..), timereportKind)
import Futurice.Time.Month
import Futurice.Tribe
import Prelude ()

import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import qualified Personio            as P
import qualified PlanMill            as PM
import qualified PlanMill.Queries    as PMQ

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data IDontKnow = IDontKnow
    { idkDate     :: !Day
    , idkName     :: !Text
    , idkTribe    :: !Tribe
    , idkCategory :: !Text
    , idkHours    :: !(NDT 'Hours Centi)
    , idkDesc     :: !Text
    }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

deriveGeneric ''IDontKnow
instance ToSchema IDontKnow where declareNamedSchema = sopDeclareNamedSchema
deriveVia [t| ToJSON IDontKnow   `Via` Sopica IDontKnow |]
deriveVia [t| FromJSON IDontKnow `Via` Sopica IDontKnow |]

data IDontKnowData = IDK !Month [IDontKnow]
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

deriveGeneric ''IDontKnowData

instance ToSchema IDontKnowData where declareNamedSchema = sopDeclareNamedSchema
deriveVia [t| ToJSON IDontKnowData   `Via` Sopica IDontKnowData |]
deriveVia [t| FromJSON IDontKnowData `Via` Sopica IDontKnowData |]

-------------------------------------------------------------------------------
-- Fetch
-------------------------------------------------------------------------------

iDontKnowData
    :: forall m. (MonadTime m, MonadPersonio m, MonadPlanMillQuery m)
    => Maybe Month
    -> m IDontKnowData
iDontKnowData mmonth = do
    today <- currentDay
    let month = fromMaybe (dayToMonth today) mmonth
    let interval = monthInterval month

    fpm0 <- personioPlanmillMap
    let fpm = HM.filter (P.employeeIsActive today . fst) fpm0

    prjs <- PMQ.projects 
    accs' <- for (toList prjs) $ \prj ->
        traverse PMQ.account (PM.pAccount prj)
    -- accTypes <- PMQ.allEnumerationValues Proxy Proxy

    let accNames :: [T.Text]
        accNames = accs' ^.. folded . folded . getter PM.saName

    idks <- ifor fpm $ \_login (p, pmu) -> do
        let uid = pmu ^. PM.identifier
        trs <- PMQ.timereports interval uid

        for (toList trs) $ \tr -> do
            task <- PMQ.task (PM.trTask tr)
            let taskName = PM.taskName task
            kind <- timereportKind tr
            let comment = fromMaybe "<empty>" $ PM.trComment tr

            if  | T.isPrefixOf "I don't know" taskName -> do
                    return $ Just IDontKnow
                        { idkDate     = PM.trStart tr
                        , idkName     = p ^. P.employeeFullname
                        , idkTribe    = p ^. P.employeeTribe
                        , idkCategory = "I don't know..."
                        , idkHours    = ndtConvert' (PM.trAmount tr)
                        , idkDesc     = comment
                        }
                | taskName /= "Sales", kind == KindInternal, any (`T.isInfixOf` comment) accNames ->
                    return $ Just IDontKnow
                        { idkDate     = PM.trStart tr
                        , idkName     = p ^. P.employeeFullname
                        , idkTribe    = p ^. P.employeeTribe
                        , idkCategory = PM.taskName task
                        , idkHours    = ndtConvert' (PM.trAmount tr)
                        , idkDesc     = comment
                        }
                | otherwise -> return Nothing

    return $ IDK month (idks ^.. folded . folded . _Just)

-------------------------------------------------------------------------------
-- Html
-------------------------------------------------------------------------------

instance ToHtml IDontKnowData where
    toHtmlRaw = toHtml
    toHtml = toHtml . renderIDontKnowData

renderIDontKnowData :: IDontKnowData -> HtmlPage "i-dont-know"
renderIDontKnowData (IDK month xs) = page_ "I don't know..." $ do
    h1_ $ "I don't know... " <> textShow month

    table_ $ do
        thead_ $ do
            th_ "Date"
            th_ "Name"
            th_ "Tribe"
            th_ "Category"
            th_ "Hours"
            th_ "Description"

        tbody_ $ for_ (sortBy (comparing idkName <> comparing idkDate) xs) $ \IDontKnow {..} -> tr_ $ do
            td_ [ class_ "nowrap" ] $ toHtml $ textShow idkDate
            td_  [ class_ "nowrap" ] $ toHtml idkName
            td_ $ toHtml idkTribe
            td_ $ toHtml idkCategory
            td_ $ toHtml idkHours
            td_ $ toHtml idkDesc
