{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Reports.DoWeStudy (
    DoWeStudy (..),
    DoWeStudyData (..),
    doWeStudyData,
    StudyKind) where

import Control.Lens              (filtered, sumOf)
import Control.Monad.Catch       (handle)
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
import Futurice.Integrations.TimereportKind
       (TimereportKind (..), timereportKind)

import qualified Data.Aeson.Lens     as L
import qualified Data.HashMap.Strict as HM
import qualified Data.Set            as Set
import qualified Data.Swagger        as Sw
import qualified Data.Text           as T
import qualified Personio            as P
import qualified PlanMill            as PM
import qualified PlanMill.Queries    as PMQ

data TrainingType = Jedi
                  | TribeInternalTraining
                  | OtherTraining
                  deriving (Eq, Enum, Bounded, Show, Generic, NFData)

data StudyKind = StudyBillable
               | StudyNonBillable
               deriving stock (Show, Eq, Enum, Bounded, Generic)
               deriving anyclass (Hashable, ToParamSchema, SopGeneric, NFData, ToSchema)
               deriving (ToHttpApiData, FromHttpApiData) via (Enumica StudyKind)

studyKindIncludesTimereportKind :: Maybe StudyKind -> TimereportKind -> Bool
studyKindIncludesTimereportKind (Just StudyBillable) tkind = tkind == KindBillable
studyKindIncludesTimereportKind (Just StudyNonBillable) tkind = tkind /= KindBillable
studyKindIncludesTimereportKind _ _ = True

instance TextEnum StudyKind where
    type TextEnumNames StudyKind =
        '["billable", "non-billable"]

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
    { dwsToday             :: !Day
    , dwsMonth             :: !Month
    , dwsSelectedTribe     :: !(Maybe Tribe)
    , dwsSelectedStudyKind :: !(Maybe StudyKind)
    , dwsData              :: [DoWeStudy]
    }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

deriveGeneric ''DoWeStudyData

instance ToSchema DoWeStudyData where declareNamedSchema = sopDeclareNamedSchema

-------------------------------------------------------------------------------
-- Fetch
-------------------------------------------------------------------------------

doWeStudyData :: forall m. (MonadTime m, MonadCatch m, MonadPersonio m, MonadPlanMillQuery m) => Maybe StudyKind -> Maybe Month -> Maybe Tribe -> m DoWeStudyData
doWeStudyData mskind mmonth mtribe = do
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

        for (toList trs) $ \tr -> handle (\PlanmillBatchError {} -> return Nothing) $ do
            kind <- timereportKind tr
            task <- PMQ.task (PM.trTask tr)
            prj <- traverse PMQ.project (PM.taskProject task)

            let comment = fromMaybe "<empty>" $ PM.trComment tr
            let commentWs = Set.fromList $ T.words $ T.toLower comment
            let otherTrainingSet = setOf (L.values . L._String) doWeStudyConfig

            if | not $ studyKindIncludesTimereportKind mskind kind -> return Nothing
               | Set.member "jedi" commentWs -> return $ Just DoWeStudy
                     { dwsDate     = PM.trStart tr
                     , dwsName     = p ^. P.employeeFullname
                     , dwsTribe    = p ^. P.employeeTribe
                     , dwsCategory = Jedi
                     , dwsProject  = maybe "<project>" PM._pName prj
                     , dwsHours    = ndtConvert' (PM.trAmount tr)
                     , dwsDesc     = comment
                     }
               | PM.taskName task == "Training" -> return $ Just DoWeStudy
                     { dwsDate     = PM.trStart tr
                     , dwsName     = p ^. P.employeeFullname
                     , dwsTribe    = p ^. P.employeeTribe
                     , dwsCategory = TribeInternalTraining
                     , dwsProject  = maybe "<project>" PM._pName prj
                     , dwsHours    = ndtConvert' (PM.trAmount tr)
                     , dwsDesc     = comment
                     }
               | not (Set.disjoint otherTrainingSet commentWs) -> return $ Just DoWeStudy
                     { dwsDate     = PM.trStart tr
                     , dwsName     = p ^. P.employeeFullname
                     , dwsTribe    = p ^. P.employeeTribe
                     , dwsCategory = OtherTraining
                     , dwsProject  = maybe "<project>" PM._pName prj
                     , dwsHours    = ndtConvert' (PM.trAmount tr)
                     , dwsDesc     = comment
                     }
               | otherwise -> return Nothing

    return $ DWS today month mtribe mskind (dwss ^.. folded . folded . _Just)

instance ToHtml TrainingType where
    toHtmlRaw = toHtml
    toHtml Jedi = toHtml ("Jedi" :: Text)
    toHtml TribeInternalTraining = toHtml ("Tribe Internal Work -> Training" :: Text)
    toHtml OtherTraining = toHtml ("General training" :: Text)

instance ToHtml DoWeStudyData where
    toHtmlRaw = toHtml
    toHtml = toHtml . renderDoWeStudyData

renderDoWeStudyData :: DoWeStudyData -> HtmlPage "do-we-study"
renderDoWeStudyData (DWS today month mtribe mskind xs) = page_ "Do we study..." $ do
    fullRow_ $ h1_ $ "Possibly learning related hour markings: "
        <> monthToText month
        <> maybe "" (\t -> " " <> tribeToText t) mtribe
        <> maybe "" (\s -> " " <> enumToText s) mskind
    when (mtribe == Nothing) $ do
        fullRow_ $ sortableTable_ $ do
            thead_ $ do
                th_ $ "Tribe"
                for_ [ minBound .. maxBound ] $ \(trainingType :: TrainingType) -> do
                    th_ $ toHtml trainingType
                th_ $ "="
            tbody_ $ do
                for_ [ minBound .. maxBound ] $ \tribe -> do
                    tr_ $ do
                        td_ $ toHtml tribe
                        for_ [ minBound .. maxBound] $ \trainingType -> do
                            td_ $ toHtml $ sumOf (folded . filtered (isRightTribeAndTrainingType tribe trainingType) . getter dwsHours) xs
                        td_ $ toHtml $ sumOf (folded . filtered (\x -> dwsTribe x == tribe) . getter dwsHours) xs

    fullRow_ $ ul_ $ do
        li_ $ toHtml (sumOf (folded . getter dwsHours) xs) <> " in total"
        li_ $ toHtml (sumOf (folded . filtered isJedi . getter dwsHours) xs) <> " related to Jedi...'"
        li_ $ toHtml (sumOf (folded . filtered isTribeInternalTraining . getter dwsHours) xs) <> " marked to Training task in Tribe Internal Work ...'"
        li_ $ do
            toHtml (sumOf (folded . filtered isOtherTraining . getter dwsHours) xs) <> " related to general training...'"
            ul_ $ li_ $ em_ $ toHtml $ "Filter keywords: " <> (T.pack . show . toList $ setOf (L.values . L._String) doWeStudyConfig)

    form_ $ div_ [ class_ "row" ] $ do
        div_ [ class_ "columns medium-3" ] $ select_ [ name_ "studykind" ] $ do
            optionSelected_ (mskind == Nothing) [value_ "-" ] "All timereports"
            optionSelected_ (mskind == Just StudyBillable) [value_ $ enumToText StudyBillable ] "Only billable reports"
            optionSelected_ (mskind == Just StudyNonBillable) [value_ $ enumToText StudyNonBillable ] "Only non-billable reports"
        div_ [ class_ "columns medium-3" ] $ select_ [ name_ "month" ] $ do
            for_ [ Month 2018 January .. succ (succ (dayToMonth today)) ] $ \m ->
                optionSelected_ (m == month) [ value_ $ monthToText m ] $ toHtml m
        div_ [ class_ "columns medium-3" ] $ select_ [ name_ "tribe" ] $ do
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
            td_ [ class_ "nowrap" ] $ toHtml dwsName
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

    isTribeInternalTraining :: DoWeStudy -> Bool
    isTribeInternalTraining DoWeStudy { dwsCategory = TribeInternalTraining } = True
    isTribeInternalTraining _ = False

    isRightTribeAndTrainingType :: Tribe -> TrainingType -> DoWeStudy -> Bool
    isRightTribeAndTrainingType tribe trainingType dws = dwsTribe dws == tribe && dwsCategory dws == trainingType
