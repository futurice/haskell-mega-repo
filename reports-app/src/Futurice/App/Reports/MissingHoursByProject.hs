{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Reports.MissingHoursByProject where

import Control.Lens              (sumOf)
import Data.Aeson.Types          (ToJSONKey)
import Futurice.Constants        (fumPublicUrl, powerPublicUrl)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Futurice.Time.Month
import Numeric.Interval.NonEmpty (inf, sup)
import Prelude ()

import Futurice.App.Reports.MissingHours

import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Tuple.Strict   as S
import qualified Personio            as P
import qualified PlanMill            as PM
import qualified Power

data MissingHoursProject = MissingHoursProject
    { mhpProjectId   :: !Power.ProjectId
    , mhpProjectName :: !Text
    , mhpTribe       :: !(Maybe Text)
    } deriving (Eq, Ord, NFData, Generic, ToSchema)

instance ToJSON MissingHoursProject
instance ToJSONKey MissingHoursProject

data MissingHoursByProject = MissingHoursByProject
    { _mhsParams         :: !MissingHoursParams
    , _mhsData           :: !(Map MissingHoursProject [StrictPair Employee :$ Vector :$ MissingHour])
    , _mhsFilterMonth    :: !(Maybe Month)
    , _mhsFilterTribe    :: !(Maybe Text)
    , _mhsWholeInterval  :: !(PM.Interval Day)
    , _mhsActiveTribes   :: !(Set Text)
    } deriving (NFData, Generic, ToSchema)

powerProjectMapping :: (MonadPower m) => m (Map Power.Project [PM.ProjectId])
powerProjectMapping = do
    projects <- Power.powerProjects
    let projects' = Map.fromList $ map (\p -> (Power.projectId p, p)) projects
    mappings <- Power.powerProjectMapping
    let mappings' = Map.fromListWith (<>)
            $ catMaybes
            $ map (\m -> (,) <$> (projects' ^.at (Power._pmPowerProjectId m)) <*> Just [Power._pmPlanMillProjectId m]) mappings
    pure mappings'

powerProjectMembers :: forall m. (MonadPower m, MonadPersonio m, MonadPlanMillQuery m)
    => PM.Interval Day
    -> m (Map Power.ProjectId (Set PM.UserId))
powerProjectMembers interval = do
    allocations <- Power.powerAllocationsByDate (Just $ inf interval) (Just $ sup interval)

    let filterCurrentAllocations = filter (\a -> utctDay (Power.allocationStartDate a) <= (sup interval) && (inf interval) <= utctDay (Power.allocationEndDate a) && Power.allocationProposed a == False)

    let allocs' = filterCurrentAllocations allocations

    persons <- Power.powerPeople
    let persons' = Map.fromList $ map (\p -> (Power.personId p, Power.personLogin p)) persons

    ppmap <- personioPlanmillMap

    pure $ Map.fromListWith (<>)
        $ map (\a -> (Power.allocationProjectId a,
                      Set.fromList $ catMaybes [Power.allocationPersonId a >>= (\p -> persons' ^.at p) >>= (\e -> PM._uId . snd <$> ppmap ^.at e)])) allocs'

missingHoursByProject
  :: forall m. (PM.MonadTime m, MonadPlanMillQuery m, MonadPersonio m, MonadPower m)
  => (PM.Interval Day -> P.Employee -> Bool)
  -> PM.Interval Day
  -> PM.Interval Day
  -> Maybe Month
  -> Maybe Text
  -> m MissingHoursByProject
missingHoursByProject predicate interval wholeInterval mmonth mtribe = do
    now <- currentTime

    tribes <- Power.powerTribes
    let tribeById = Map.fromList $ map (\t -> (Power.tribeId t, Power.tribeName t)) tribes
    let tribeByName = Map.fromList $ map (\t -> (Power.tribeName t, Power.tribeId t)) tribes

    powerProjects <- Power.powerProjects
    let powerProjects' = maybe
                         powerProjects
                         (\tribeId -> filter (\p -> Power.projectTribeId p == tribeId) powerProjects)
                         (mtribe >>= (\t -> tribeByName ^.at t))
    let powerProjects'' = Map.fromList $ map (\p -> (Power.projectId p, p)) $ powerProjects'

    members <- powerProjectMembers interval
    let members' = Map.fromList $
                   catMaybes $
                   map (\(pid,ms) -> powerProjects'' ^.at pid >>= \p -> Just (toMissingHourProject p (tribeById ^. at (Power.projectTribeId p)), ms)) $
                   Map.toList members

    fpm0 <- personioPlanmillMap
    let fpm0' = HM.fromList $ map (\(_,(pemp,plemp)) -> (PM._uId plemp,(pemp,plemp))) $ HM.toList fpm0
    let fpm1 = HM.filter (predicate interval . fst) fpm0'

    fpm1' <- traverse (uncurry perUser) fpm1
    let total = sumOf (folded . folded . folded . missingHourCapacity) fpm1'

    let fpm3 = Map.mapWithKey (\_ ms -> mapMaybe (\m -> fpm1' ^.at m) $ toList ms) members'

    pure $ MissingHoursByProject
        (MissingHoursParams now (inf interval) (sup interval) total (fumPublicUrl <> "/"))
        fpm3
        mmonth
        mtribe
        wholeInterval
        (Set.fromList $ Power.tribeName <$> tribes)
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

    toMissingHourProject project tribeName = MissingHoursProject
        { mhpProjectId   = Power.projectId project
        , mhpProjectName = Power.projectName project
        , mhpTribe       = tribeName
        }

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
                toHtml (dayToMonth $ inf wholeInterval)
                <> toHtml (tid " - ")
                <> toHtml (dayToMonth $ sup wholeInterval)
            for_ [ (dayToMonth $ inf wholeInterval ) .. (dayToMonth $ sup wholeInterval) ] $ \m ->
              optionSelected_ (Just m == mmonth) [ value_ $ monthToText m ] $ toHtml m
        div_ [ class_ "columns medium-3" ] $ select_ [ name_ "tribe", data_ "futu-id" "missing-hours-tribe" ] $ do
            optionSelected_ (mtribe == Nothing) [ value_ "-"] "All tribes"
            for_ (sort $ toList activeTribes) $ \tribe ->
                optionSelected_ (Just tribe == mtribe) [value_ $ tribe ] $ toHtml tribe
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
                    rows [projectToHtml project, toHtml (fromMaybe "" $ mhpTribe project)] $
                    (flip map) empList' $ \(employee S.:!: vecHours) -> do
                        td_ $ toHtml $ employeeName employee
                        td_ $ toHtml $ sum $ _missingHourCapacity <$> vecHours
  where
      tid :: Text -> Text
      tid = id

      projectToHtml :: (Monad m) => MissingHoursProject -> HtmlT m ()
      projectToHtml p = do
          let t = textShow $ mhpProjectId p
          a_ [ class_ "power", href_ $ powerPublicUrl <> "/v3/projects/" <> t ] $ toHtml $ mhpProjectName p



rows :: (Monad m, Foldable f) => [HtmlT m ()] -> f (HtmlT m ()) -> HtmlT m ()
rows x ys = case toList ys of
    []      -> tr_ $ traverse_ td_ x >> td_ mempty
    (y:ys') -> do
        tr_ $ traverse_ (td_ [ rowspan_ $ textShow $ succ $ length ys' ]) x >> y
        for_ ys' $ \y' -> tr_ y'
