{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Reports.MissingHoursByProject where

import Control.Lens              (sumOf)
import Control.Monad             (filterM)
import Data.Aeson.Types          (ToJSONKey)
import Futurice.Constants        (fumPublicUrl)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Futurice.Time
import Futurice.Time.Month
import Futurice.Tribe
import Numeric.Interval.NonEmpty (Interval, inf, sup)
import Prelude ()

import Futurice.App.Reports.MissingHours

import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Tuple.Strict   as S
import qualified Data.Vector         as V
import qualified Personio            as P
import qualified PlanMill            as PM
import qualified PlanMill.Queries    as PMQ

data MissingHoursProject = MissingHoursProject
    { mhpProjectId   :: !PM.ProjectId
    , mhpProjectName :: !Text
    , mhpTribe       :: !(Maybe Text)
    } deriving (Eq, Ord, NFData, Generic, ToSchema)

instance ToJSON MissingHoursProject
instance ToJSONKey MissingHoursProject

data MissingHoursByProject = MissingHoursByProject
    { _mhsParams         :: !MissingHoursParams
    , _mhsData           :: !(Map MissingHoursProject [StrictPair Employee :$ Vector :$ MissingHour])
    , _mhsFilterMonth    :: !(Maybe Month)
    , _mhsFilterTribe    :: !(Maybe Tribe)
    , _mhsWholeInterval  :: !(PM.Interval Day)
    , _mhsActiveTribes   :: !(Set Tribe)
    } deriving (NFData, Generic, ToSchema)

missingHoursByProject
  :: forall m. (PM.MonadTime m, MonadPlanMillQuery m, MonadPersonio m)
  => (PM.Interval Day -> P.Employee -> Bool)
  -> PM.Interval Day
  -> PM.Interval Day
  -> Maybe Month
  -> Maybe Tribe
  -> m MissingHoursByProject
missingHoursByProject predicate interval wholeInterval mmonth mtribe = do
    now <- currentTime

    projects <- PMQ.projects
    portfolioMap <- toPortfolioMap <$> PMQ.portfolios
    let tribes = ([minBound .. maxBound] :: [Tribe])
    let activeProjects' = toList $ V.filter (\p -> PM.pStart p <= Just now && Just now <= PM.pFinish p) projects
    let removeInternalProjects project = do
            projectCategory <- (`PMQ.enumerationValue` "C?") ((^. PM.pCategory) project)
            let portfolio = PM._pPortfolioId project >>= \pid -> portfolioMap ^.at pid
            pure $ projectCategory /= "Team internal work"
                && projectCategory /= "Futurice internal"
                && ((PM._portfolioProjectType <$> portfolio) /= Just PM.InternalProject)
    activeProjects <- filterM removeInternalProjects activeProjects'
    let membersForProject p = do
            members <- PMQ.projectMembers (p ^. PM.pId)
            pure (toMissingHourProject portfolioMap p, Set.fromList $ map PM._projectMemberUserId $ V.toList members)
    membersMap <- Map.fromList <$> traverse membersForProject activeProjects

    fpm0 <- personioPlanmillMap
    let fpm0' = HM.fromList $ map (\(_,(pemp,plemp)) -> (PM._uId plemp,(pemp,plemp))) $ HM.toList fpm0
    let fpm1 = HM.filter (predicate interval . fst) fpm0'

    fpm1' <- traverse (uncurry perUser) fpm1
    let fpm2 = maybe fpm1' (\emp -> HM.filter (\(e S.:!: _) -> employeeTribe e == emp) fpm1') mtribe
    let total = sumOf (folded . folded . folded . missingHourCapacity) fpm2

    let fpm3 = Map.mapWithKey (\_ members -> mapMaybe (\m -> fpm2 ^.at m) $ toList members) membersMap

    pure $ MissingHoursByProject
        (MissingHoursParams now (inf interval) (sup interval) total (fumPublicUrl <> "/"))
        fpm3
        mmonth
        mtribe
        wholeInterval
        (activeTribes $ fst <$> HM.elems fpm0)
  where
    perUser :: P.Employee -> PM.User -> m (StrictPair Employee :$ Vector :$ MissingHour)
    perUser pEmployee pmUser = (S.:!:)
        <$> planmillEmployee (pmUser ^. PM.identifier)
        <*> missingHoursForUser interval' pmUser
      where
        -- shrink interval with end date, if it exists
        interval' = i PM.... s
        i = inf interval & mcase (pEmployee ^. P.employeeHireDate) id max
        s = sup interval & mcase (pEmployee ^. P.employeeEndDate) id min

    -- All tribes that currently working employees are in
    activeTribes :: [P.Employee] -> Set Tribe
    activeTribes emp = Set.fromList $ map (^. P.employeeTribe) $ filter (\e -> e ^. P.employeeStatus /= P.Inactive) emp

    toMissingHourProject portfolioMap p = MissingHoursProject
        { mhpProjectId   = p ^. PM.pId
        , mhpProjectName = p ^. PM.pName
        , mhpTribe       = textShow <$> (PM._pPortfolioId p >>= (\pid -> (portfolioMap ^.at pid)) >>= Just . PM._portfolioName)
        }

    toPortfolioMap :: PM.Portfolios -> Map PM.PortfolioId PM.Portfolio
    toPortfolioMap (PM.Portfolios portfolios) = Map.fromList $ (\p -> (PM._portfolioId p, p)) <$> V.toList portfolios

instance ToHtml MissingHoursByProject where
    toHtmlRaw = toHtml
    toHtml = toHtml . renderMissingHoursByProject

renderMissingHoursByProject :: MissingHoursByProject -> HtmlPage "missing-hours-project"
renderMissingHoursByProject (MissingHoursByProject params data' mmonth mtribe wholeInterval activeTribes) = page_ "Missing hours" $ do
    fullRow_ $ h1_ "Missing hours markings (internal; salary; Monthly)"
    fullRow_ $ toHtml params
    form_ $ div_ [ class_ "row" ] $ do
        div_ [ class_ "columns medium-3" ] $ select_ [ name_ "month", data_ "futu-id" "missing-hours-month" ] $ do
            optionSelected_ (mmonth == Nothing) [ value_ "-"] $
                toHtml (dayToMonth $ params ^. mhpFromDay)
                <> toHtml (tid " - ")
                <> toHtml (dayToMonth $ params ^. mhpToDay)
            for_ [ (dayToMonth $ inf wholeInterval ) .. (dayToMonth $ sup wholeInterval) ] $ \m ->
              optionSelected_ (Just m == mmonth) [ value_ $ monthToText m ] $ toHtml m
        div_ [ class_ "columns medium-3" ] $ select_ [ name_ "tribe", data_ "futu-id" "missing-hours-tribe" ] $ do
            optionSelected_ (mtribe == Nothing) [ value_ "-"] "All tribes"
            for_ (sortOn tribeToText $ toList activeTribes) $ \tribe ->
                optionSelected_ (Just tribe == mtribe) [value_ $ tribeToText tribe ] $ toHtml tribe
        div_ [ class_ "columns medium-2" ] $ input_ [ class_ "button", type_ "submit", value_ "Update" ]

    fullRow_ $ do
        h2_ "Projects with people with missing hours"
        table_ [data_ "futu-id" "missing-hours-by-project-table"] $ do
            thead_ $ do
                th_ "Project name"
                th_ "Tribe"
                th_ "Employee name"
                th_ "Hours"
            tbody_ $ for_ (sortOn (mhpProjectName . fst) $ Map.toList data') $ \(project, empList) -> do
                let empList' = filter (\(_ S.:!: vecHours) -> ((sum $ _missingHourCapacity <$> vecHours) > 0)) empList
                when (length empList' > 0) $
                    rows [toHtml (mhpProjectName project), toHtml (fromMaybe "" $ mhpTribe project)] $
                    (flip map) empList' $ \(employee S.:!: vecHours) -> do
                        td_ $ toHtml $ employeeName employee
                        td_ $ toHtml $ sum $ _missingHourCapacity <$> vecHours
  where
      tid :: Text -> Text
      tid = id


rows :: (Monad m, Foldable f) => [HtmlT m ()] -> f (HtmlT m ()) -> HtmlT m ()
rows x ys = case toList ys of
    []      -> tr_ $ traverse_ td_ x >> td_ mempty
    (y:ys') -> do
        tr_ $ traverse_ (td_ [ rowspan_ $ textShow $ succ $ length ys' ]) x >> y
        for_ ys' $ \y' -> tr_ y'
