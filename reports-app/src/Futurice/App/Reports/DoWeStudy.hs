{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Reports.DoWeStudy (
    DoWeStudy (..),
    DoWeStudyData (..),
    doWeStudyData) where

import Control.Lens              (filtered, sumOf)
import Data.Fixed                (Centi)
import Data.Ord                  (comparing)
import Data.Set.Lens             (setOf)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Futurice.Time
import Futurice.Time.Month
import Futurice.Tribe
import Prelude ()

import qualified Data.Aeson.Lens     as L
import qualified Data.HashMap.Strict as HM
import qualified Data.Set            as Set
import qualified Data.Swagger        as Sw
import qualified Data.Text           as T
import qualified Personio            as P
import qualified PlanMill            as PM
import qualified PlanMill.Queries    as PMQ

data TrainingType = Jedi
                  | OtherTraining
                  | Test
                  deriving (Show, Generic, NFData)

doWeStudyConfig :: Value
doWeStudyConfig = $(makeRelativeToProject "do-we-study.json" >>= embedFromJSON (Proxy :: Proxy Value))

instance ToSchema TrainingType where
    declareNamedSchema = Sw.genericDeclareNamedSchemaUnrestricted Sw.defaultSchemaOptions

data DoWeStudy = DoWeStudy
    { dwsDate     :: !Day
    , dwsName     :: !Text
    , dwsTribe    :: !Tribe
    , dwsCategory :: !TrainingType
    , dwsProject  :: !Text
    , dwsHours    :: !(NDT 'Hours Centi)
    , dwsDesc     :: !Text
    }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

deriveGeneric ''DoWeStudy
instance ToSchema DoWeStudy where declareNamedSchema = sopDeclareNamedSchema

data DoWeStudyData = DWS
    { dwsToday         :: !Day
    , dwsMonth         :: !Month
    , dwsSelectedTribe :: !(Maybe Tribe)
    , dwsData          :: [DoWeStudy]
    }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

deriveGeneric ''DoWeStudyData

instance ToSchema DoWeStudyData where declareNamedSchema = sopDeclareNamedSchema

-------------------------------------------------------------------------------
-- Fetch
-------------------------------------------------------------------------------

doWeStudyData :: forall m. (MonadTime m, MonadPersonio m, MonadPlanMillQuery m) => Maybe Month -> Maybe Tribe -> m DoWeStudyData
doWeStudyData mmonth mtribe = do
    today <- currentDay
    let month = fromMaybe (dayToMonth today) mmonth
    let interval = monthInterval month

    fpm0 <- personioPlanmillMap

    let fpm1 = HM.filter (P.employeeIsActive today . fst) fpm0
    let fpm = case mtribe of
            Nothing    -> fpm1
            Just tribe -> HM.filter (\e -> e ^. _1 . P.employeeTribe == tribe) fpm1

    dwss <- ifor fpm $ \_login (p, pmu) -> do
        let uid = pmu ^. PM.identifier
        trs <- PMQ.timereports interval uid

        for (toList trs) $ \tr -> do
            task <- PMQ.task (PM.trTask tr)
            prj <- traverse PMQ.project (PM.taskProject task)
            let comment = fromMaybe "<empty>" $ PM.trComment tr
            let commentWs = Set.fromList $ T.words $ T.toLower comment
            let otherTrainingSet = setOf (L.values . L._String) doWeStudyConfig

            if | Set.member "jedi" commentWs -> do
                     return $ Just DoWeStudy
                         { dwsDate     = PM.trStart tr
                         , dwsName     = p ^. P.employeeFullname
                         , dwsTribe    = p ^. P.employeeTribe
                         , dwsCategory = Jedi
                         , dwsProject  = maybe "<project>" PM.pName prj
                         , dwsHours    = ndtConvert' (PM.trAmount tr)
                         , dwsDesc     = comment
                         }
               | not (Set.disjoint otherTrainingSet commentWs) -> do
                     return $ Just DoWeStudy
                         { dwsDate     = PM.trStart tr
                         , dwsName     = p ^. P.employeeFullname
                         , dwsTribe    = p ^. P.employeeTribe
                         , dwsCategory = OtherTraining
                         , dwsProject  = maybe "<project>" PM.pName prj
                         , dwsHours    = ndtConvert' (PM.trAmount tr)
                         , dwsDesc     = comment
                         }
               | otherwise -> return $ Just DoWeStudy
                         { dwsDate     = PM.trStart tr
                         , dwsName     = p ^. P.employeeFullname
                         , dwsTribe    = p ^. P.employeeTribe
                         , dwsCategory = Test
                         , dwsProject  = maybe "<project>" PM.pName prj
                         , dwsHours    = ndtConvert' (PM.trAmount tr)
                         , dwsDesc     = comment
                         }

    return $ DWS today month mtribe (dwss ^.. folded . folded . _Just)

instance ToHtml TrainingType where
    toHtmlRaw = toHtml
    toHtml Jedi = toHtml ("Jedi" :: Text)
    toHtml OtherTraining = toHtml ("General training" :: Text)
    toHtml Test = toHtml ("Test" :: Text)

instance ToHtml DoWeStudyData where
    toHtmlRaw = toHtml
    toHtml = toHtml . renderDoWeStudyData

renderDoWeStudyData :: DoWeStudyData -> HtmlPage "do-we-study"
renderDoWeStudyData (DWS today month mtribe xs) = page_ "Do we study..." $ do
    fullRow_ $ h1_ $ "Possibly learning related hour markings: "
        <> monthToText month
        <> maybe "" (\t -> " " <> tribeToText t) mtribe

    fullRow_ $ ul_ $ do
        li_ $ toHtml (sumOf (folded . getter dwsHours) xs) <> " hours in total"
        li_ $ toHtml (sumOf (folded . filtered isJedi . getter dwsHours) xs) <> " hours related to Jedi...'"
        li_ $ toHtml (sumOf (folded . filtered isOtherTraining . getter dwsHours) xs) <> " hours related to general training...'"

    form_ $ div_ [ class_ "row" ] $ do
        div_ [ class_ "columns medium-5" ] $ select_ [ name_ "month" ] $ do
            for_ [ Month 2018 January .. succ (succ (dayToMonth today)) ] $ \m ->
                optionSelected_ (m == month) [ value_ $ monthToText m ] $ toHtml m
        div_ [ class_ "columns medium-5" ] $ select_ [ name_ "tribe" ] $ do
            optionSelected_ (mtribe == Nothing) [ value_ "-"] "All tribes"
            for_ [ minBound .. maxBound ] $ \tribe ->
                optionSelected_ (mtribe ==  Just tribe) [value_ $ tribeToText tribe ] $ toHtml tribe
        div_ [ class_ "columns medium-2" ] $ input_ [ class_ "button", type_ "submit", value_ "Update" ]

    fullRow_ $ sortableTable_ $ do
        thead_ $ do
            th_ "Date"
            th_ "Name"
            th_ "Tribe"
            th_ "Training type"
            th_ "Project"
            th_ "Hours"
            th_ "Description"

        tbody_ $ for_ (sortBy (comparing dwsName <> comparing dwsDate) xs) $ \DoWeStudy {..} -> tr_ $ do
            td_ [ class_ "nowrap" ] $ toHtml $ textShow dwsDate
            td_  [ class_ "nowrap" ] $ toHtml dwsName
            td_ $ toHtml dwsTribe
            td_ $ toHtml dwsCategory
            td_ $ toHtml dwsProject
            td_ $ toHtml dwsHours
            td_ $ wordsToHtml (T.words dwsDesc)
  where
    wordsToHtml [] = pure ()
    wordsToHtml (w:ws)
        | Set.member (T.toLower w) (Set.insert "jedi" $ setOf (L.values . L._String) doWeStudyConfig) = span_ [ class_ "label success" ] (toHtml w) <> " " <> wordsToHtml ws
        | otherwise                                                               = toHtml w <> " " <> wordsToHtml ws

    isJedi :: DoWeStudy -> Bool
    isJedi DoWeStudy { dwsCategory = Jedi } = True
    isJedi _ = False

    isOtherTraining :: DoWeStudy -> Bool
    isOtherTraining DoWeStudy { dwsCategory = OtherTraining } = True
    isOtherTraining _ = False
