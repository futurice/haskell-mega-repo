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
    IDontKnowData (..),
    IDontKnow (..),
    Category (..),
    ) where

import Data.Aeson                           (Value)
import Data.Fixed                           (Centi)
import Data.Ord                             (comparing)
import Data.Set.Lens                        (setOf)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Integrations.TimereportKind
       (TimereportKind (..), timereportKind)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Futurice.Time
import Futurice.Time.Month
import Futurice.Tribe
import Prelude ()

import qualified Data.Aeson.Lens as L
import qualified Data.HashMap.Strict as HM
import qualified Data.Set            as Set
import qualified Data.Text           as T
import qualified Personio            as P
import qualified Data.Swagger as Sw
import qualified PlanMill            as PM
import qualified PlanMill.Queries    as PMQ

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data Category
    = CatIDontKnow
    | CatICantMark
    | CatInternal !Text
  deriving stock (Show, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

instance ToSchema Category where
    declareNamedSchema = Sw.genericDeclareNamedSchemaUnrestricted Sw.defaultSchemaOptions

data IDontKnow = IDontKnow
    { idkDate     :: !Day
    , idkName     :: !Text
    , idkTribe    :: !Tribe
    , idkCategory :: !Category
    , idkProject  :: !Text
    , idkHours    :: !(NDT 'Hours Centi)
    , idkDesc     :: !Text
    }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

deriveGeneric ''IDontKnow
instance ToSchema IDontKnow where declareNamedSchema = sopDeclareNamedSchema
deriveVia [t| ToJSON IDontKnow   `Via` Sopica IDontKnow |]
deriveVia [t| FromJSON IDontKnow `Via` Sopica IDontKnow |]

data IDontKnowData = IDK !Day !Month (Set Text) [IDontKnow]
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
    accTypes <- PMQ.allEnumerationValues Proxy Proxy

    accs' <- for (toList prjs) $ \prj -> do
        acc <- for (PM.pAccount prj) $ \accId -> do
            acc <- PMQ.account accId
            if accTypes ^? ix (PM.saType acc) == Just "My Company"
            then return Nothing
            else return (Just acc)
        return (join acc)

    let accNames' :: Set T.Text
        accNames' = setOf
            (folded . folded . getter (take 1 . filter (\w -> T.length w > 2) . T.words . T.toLower . PM.saName) . folded)
            accs'

        accNames :: Set T.Text
        accNames = Set.difference accNames' excludes
          where
            excludes = setOf
                (L.key "exclude" . L.values . L._String)
                iDontKnowConfig

    idks <- ifor fpm $ \_login (p, pmu) -> do
        let uid = pmu ^. PM.identifier
        trs <- PMQ.timereports interval uid

        for (toList trs) $ \tr -> do
            task <- PMQ.task (PM.trTask tr)
            prj <- traverse PMQ.project (PM.taskProject task)
            let taskName = PM.taskName task
            kind <- timereportKind tr
            let comment = fromMaybe "<empty>" $ PM.trComment tr
            let commentWs = Set.fromList $ T.words $ T.toLower comment

            if  | T.isPrefixOf "I don't know" taskName -> do
                    return $ Just IDontKnow
                        { idkDate     = PM.trStart tr
                        , idkName     = p ^. P.employeeFullname
                        , idkTribe    = p ^. P.employeeTribe
                        , idkCategory = CatIDontKnow
                        , idkProject  = maybe "<project>" PM.pName prj
                        , idkHours    = ndtConvert' (PM.trAmount tr)
                        , idkDesc     = comment
                        }
                | T.isPrefixOf "I can't mark" taskName -> do
                    return $ Just IDontKnow
                        { idkDate     = PM.trStart tr
                        , idkName     = p ^. P.employeeFullname
                        , idkTribe    = p ^. P.employeeTribe
                        , idkCategory = CatICantMark
                        , idkProject  = maybe "<project>" PM.pName prj
                        , idkHours    = ndtConvert' (PM.trAmount tr)
                        , idkDesc     = comment
                        }
                | not ("sales" `T.isInfixOf` T.toLower taskName), kind == KindInternal, not (Set.disjoint commentWs accNames) ->
                    return $ Just IDontKnow
                        { idkDate     = PM.trStart tr
                        , idkName     = p ^. P.employeeFullname
                        , idkTribe    = p ^. P.employeeTribe
                        , idkCategory = CatInternal $ PM.taskName task
                        , idkProject  = maybe "<project>" PM.pName prj
                        , idkHours    = ndtConvert' (PM.trAmount tr)
                        , idkDesc     = comment
                        }
                | otherwise -> return Nothing

    return $ IDK today month accNames (idks ^.. folded . folded . _Just)

iDontKnowConfig :: Value
iDontKnowConfig = $(makeRelativeToProject "i-dont-know.json" >>= embedFromJSON (Proxy :: Proxy Value))

-------------------------------------------------------------------------------
-- Html
-------------------------------------------------------------------------------

instance ToHtml IDontKnowData where
    toHtmlRaw = toHtml
    toHtml = toHtml . renderIDontKnowData

renderIDontKnowData :: IDontKnowData -> HtmlPage "i-dont-know"
renderIDontKnowData (IDK today month accNames xs) = page_ "I don't know..." $ do
    fullRow_ $ h1_ $ "I don't know... " <> monthToText month

    form_ $ div_ [ class_ "row" ] $ do
        div_ [ class_ "columns medium-10" ] $ select_ [ name_ "month" ] $ do
            -- TODO self update
            for_ [ Month 2018 January .. succ (succ (dayToMonth today)) ] $ \m ->
                optionSelected_ (m == month) [ value_ $ monthToText m ] $ toHtml m
        div_ [ class_ "columns medium-2" ] $ input_ [ class_ "button", type_ "submit", value_ "Update" ]

    fullRow_ $ sortableTable_ $ do
        thead_ $ do
            th_ "Date"
            th_ "Name"
            th_ "Tribe"
            th_ "Category"
            th_ "Project"
            th_ "Hours"
            th_ "Description"

        tbody_ $ for_ (sortBy (comparing idkName <> comparing idkDate) xs) $ \IDontKnow {..} -> tr_ $ do
            td_ [ class_ "nowrap" ] $ toHtml $ textShow idkDate
            td_  [ class_ "nowrap" ] $ toHtml idkName
            td_ $ toHtml idkTribe
            td_ $ case idkCategory of
                CatIDontKnow -> span_ [ class_ "label warning" ] "I don't know..."
                CatICantMark -> span_ [ class_ "label alert"   ] "I can't mark..."
                CatInternal t -> toHtml t
            td_ $ toHtml idkProject
            td_ $ toHtml idkHours
            td_ $ wordsToHtml (T.words idkDesc)
  where
    wordsToHtml [] = pure ()
    wordsToHtml (w:ws)
        | Set.member (T.toLower w) accNames = span_ [ class_ "label success" ] (toHtml w) <> " " <> wordsToHtml ws
        | otherwise                         = toHtml w <> " " <> wordsToHtml ws
