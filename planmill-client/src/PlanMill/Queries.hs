{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- | High-level query api
--
-- See <https://developers.planmill.com/api/>
module PlanMill.Queries (
    -- * Monad class
    MonadPlanMillQuery,
    -- * Special queries
    timereports,
    allTimereports,
    capacities,
    enumerationValue,
    allEnumerationValues,
    -- * Non special queries
    me,
    user,
    users,
    team,
    userTimebalance,
    absences,
    account,
    assignments,
    project,
    projectMembers,
    projects,
    projectTasks,
    task,
    capacitycalendars,
    allRevenuesReport,
    valueCreationByMonthReport,
    teamsHoursByCategoryReport,
    earnedVacationsReport,
    -- * Queries
    usersQuery,
    absencesQuery,
    -- timereportsModifiedQuery,
    --- * Raw queries
    project',
    projects',
    projectsWithType,
    simpleProject,
    portfolios,
    invoices
    ) where

import PlanMill.Internal.Prelude

import Control.Lens          (alongside, withIndex)
import Control.Monad.Memoize (MonadMemoize (memo))
import Data.Constraint       (Dict (..))
import Data.List             (find)
import Data.Reflection       (reifySymbol)

import Control.Monad.PlanMill
import Futurice.Time.Month           (Month (..))
import PlanMill.Types
       (Absence, Absences, Account, AccountId, AllRevenues2, Assignments,
       CapacityCalendars, EarnedVacations, InvoiceDatas, Me,
       PersonValueCreations, Portfolios, Project (..), ProjectId,
       ProjectMembers, Projects, SimpleProject, Task, TaskId, Tasks, Team,
       TeamId, TeamsHoursByCategory, TimeBalance, Timereport, Timereports, User,
       UserCapacities, UserId, Users, ViewTemplate (..), getPortfolio,
       identifier, sProject, viewTemplateToInt)
import PlanMill.Types.Enumeration
import PlanMill.Types.Meta           (Meta, lookupFieldEnum)
import PlanMill.Types.Query          (Query (..), QueryTag (..))
import PlanMill.Types.ResultInterval (elimInterval)
import PlanMill.Types.UOffset        (showPlanmillUTCTime)
import PlanMill.Types.UrlPart        (UrlParts, toUrlParts, (//))

import qualified Data.IntMap.Strict as IM
import qualified Data.Map           as Map

-- | Get timereports for interval and user.
timereports :: MonadPlanMillQuery m => Interval Day -> UserId -> m Timereports
timereports i u = planmillVectorQuery (QueryTimereports (Just i) u)

-- | All timereports for given user
allTimereports ::  MonadPlanMillQuery m => UserId -> m Timereports
allTimereports u = planmillVectorQuery (QueryTimereports Nothing u)

-- | Get capacities for interval and user.
capacities :: MonadPlanMillQuery m => Interval Day -> UserId -> m UserCapacities
capacities i u = planmillVectorQuery (QueryCapacities i u)

-------------------------------------------------------------------------------
-- Enumeration
-------------------------------------------------------------------------------

enumerationValue
    :: forall entity field m.
        ( HasMeta entity
        , KnownSymbol field
        , MonadPlanMillQuery m, MonadMemoize m
        )
    => EnumValue entity field
    -> Text  -- ^ Default text
    -> m Text
enumerationValue (EnumValue value) defaultText = do
    mDesc <- enumerationForField (Proxy :: Proxy entity) (Proxy :: Proxy field)
    case mDesc of
        Nothing   -> return defaultText
        Just (MkSomeEnumDesc (EnumDesc im)) -> case IM.lookup value im of
            Nothing        -> return defaultText
            Just textValue -> return textValue

allEnumerationValues
    :: (HasMeta entity , KnownSymbol field , MonadPlanMillQuery m, MonadMemoize m)
    => Proxy entity -> Proxy field
    -> m (Map (EnumValue entity field) Text)
allEnumerationValues pe pf = mk <$> enumerationForField pe pf
  where
    mk :: Maybe SomeEnumDesc -> Map (EnumValue entity field) Text
    mk Nothing = mempty
    mk (Just (MkSomeEnumDesc (EnumDesc im))) = toMapOf
        (ifolded . withIndex . alongside (getter EnumValue) id . ifolded)
        im

enumerationForField
    :: forall entity field m.
        ( HasMeta entity
        , KnownSymbol field
        , MonadPlanMillQuery m, MonadMemoize m
        )
    => Proxy entity -> Proxy field
    -> m (Maybe SomeEnumDesc)
enumerationForField entityProxy fieldNameProxy = memo (EVM :: EVM entity field ) $ do
    m <- meta entityProxy
    case lookupFieldEnum m (symbolVal fieldNameProxy ^. packed) of
        Nothing -> return Nothing -- TODO: Throw an unknown field exception?
        Just enumName  -> reifyTextSymbol enumName e
  where
    e :: forall k. KnownSymbol k => Proxy k -> m (Maybe SomeEnumDesc)
    e enumProxy = case instFSymbol :: Dict (MonadPlanMillC m (EnumDesc k)) of
        Dict -> do
            desc <- enumerations enumProxy
            return $ Just $ MkSomeEnumDesc desc

-- enumerationValue memo key
data EVM (entity :: *) (field :: Symbol) = EVM
  deriving stock (Eq, Generic)
  deriving anyclass (Hashable)

-- | View details of single enumeration.
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#enumerations_get>
enumerations
    :: forall m k. (MonadPlanMillQuery m, KnownSymbol k)
    => Proxy k -> m (EnumDesc k)
enumerations p =
    case instFSymbol :: Dict (MonadPlanMillC m (EnumDesc k)) of
        Dict -> planmillQuery
            $ QueryGet (QueryTagEnumDesc p) qs
            $ toUrlParts ("enumerations" :: Text)
  where
    qs = Map.fromList [ ("name" :: Text, symbolVal p ^. packed :: Text) ]

-------------------------------------------------------------------------------
-- Non special queries
-------------------------------------------------------------------------------

{-
 - TODO: Planmill doesn't support this yet
timereportsModifiedQuery
    :: UserId
    -> UTCTime
    -> UTCTime
    -> Query Timereports
timereportsModifiedQuery (Ident uid) mi ma =
    QueryPagedGet QueryTagTimereport (qs ++ qs') ps
  where
    qs  = intervalToQueryString $ ResultInterval IntervalModified (mi ... ma)
    qs' = [ ("person",  show uid ^. packed) ]
    ps  = toUrlParts ("timereports" :: Text)
-}

-- | View details of single me.
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#me_get>
me :: MonadPlanMillQuery m => m Me
me = planmillQuery
    $ QueryGet QueryTagMe mempty
    $ toUrlParts ("me" :: Text)

-- | Get a list of users
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#users_get>
users :: MonadPlanMillQuery m => m Users
users = planmillVectorQuery usersQuery

usersQuery :: Query Users
usersQuery = QueryPagedGet QueryTagUser mempty $ toUrlParts ("users" :: Text)

-- | A single user in PlanMill
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#users__id__get>
user :: MonadPlanMillQuery m => UserId -> m User
user uid = planmillQuery
    $ QueryGet QueryTagUser mempty
    $ toUrlParts $ ("users" :: Text) // uid

-- | A single team in PlanMill
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#teams__id__get>
team :: MonadPlanMillQuery m => TeamId -> m Team
team tid = planmillQuery
    $ QueryGet QueryTagTeam mempty
    $ toUrlParts $ ("teams" :: Text) // tid

-- | A single timebalance in PlanMill. This is a read-only item
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#users__id__timebalance_get>
userTimebalance :: MonadPlanMillQuery m => UserId -> m TimeBalance
userTimebalance uid = planmillQuery
    $ QueryGet QueryTagTimebalance mempty
    $ toUrlParts $ ("users" :: Text) // uid // ("timebalance" :: Text)

-- | Get a list of absences.
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#absences_get>
absences :: MonadPlanMillQuery m => m Absences
absences = planmillVectorQuery absencesQuery

absencesQuery :: Query Absences
absencesQuery = QueryPagedGet QueryTagAbsence mempty $ toUrlParts $ ("absences" :: Text)

-- | View details of a single account.
--
-- See <https://developers.planmill.com/api/#accounts__account_id__get>
account :: MonadPlanMillQuery m => AccountId -> m Account
account aid = planmillQuery
    $ QueryGet QueryTagAccount mempty
    $ toUrlParts $ ("accounts" :: Text) // aid

-- | A single project in PlanMill.
--
-- Under the hoods asks for all projects listing, and the individual projects.
-- Combines the data        .
--
project :: MonadPlanMillQuery m => ProjectId -> m Project
project pid = memo pid $ do
    p <- project' pid
    ps <- projects'
    return $ case find (\p' -> p' ^. identifier == pid) ps of
        Nothing -> p
        Just p' -> combineProjects p p'

-- | Get a list of projects.
--
-- Under the hood, asks for individual projects too.
--
projects :: MonadPlanMillQuery m => m Projects
projects = do
    ps <- projects'
    for ps $ \p -> do
        p' <- project' (p ^. identifier)
        return $ combineProjects p p'

combineProjects :: Project -> Project -> Project
combineProjects p p' = Project
    { _pId             = p ^. identifier
    , _pName            = _pName p
    , _pAccount         = combineMaybe _pAccount p p'
    , pAccountName     = combineMaybe pAccountName p p'
    , _pCategory        = _pCategory p
    , _pOperationalId   = combineMaybe _pOperationalId p p'
    , _pPortfolioId     = combineMaybe _pPortfolioId p p'
    , pStart           = combineMaybe pStart p p'
    , pFinish          = combineMaybe pFinish p p'
    , pProjectManager  = combineMaybe pProjectManager p p'
    , pInvoicedRevenue = combineMax pInvoicedRevenue p p'
    , pActualRevenue   = combineMax pActualRevenue p p'
    , pTotalRevenue    = combineMax pTotalRevenue p p'
    , pActualCost      = combineMax pActualCost p p'
    , pTotalCost       = combineMax pTotalCost p p'
    , pActualEffort    = combineMax pActualEffort p p'
    , pTotalEffort     = combineMax pTotalEffort p p'
    }
  where
    combineMax :: Ord b => (a -> b) -> a -> a -> b
    combineMax f x y = max (f x) (f y)

    combineMaybe :: (a -> Maybe b) -> a -> a -> Maybe b
    combineMaybe f x y = f x <|> f y

-- | A single project in PlanMill.
--
-- See <https://developers.planmill.com/api/#projects__project_id__get>
project' :: MonadPlanMillQuery m => ProjectId -> m Project
project' pid = planmillQuery
    $ QueryGet QueryTagProject mempty
    $ toUrlParts $ ("projects" :: Text) // pid

simpleProject :: MonadPlanMillQuery m => ProjectId -> m SimpleProject
simpleProject pid = do
    p <- project' pid
    pure $ p ^. sProject

-- | Get a list of projects.
--
-- See <https://developers.planmill.com/api/#projects_get>
projects' :: MonadPlanMillQuery m => m Projects
projects' = planmillVectorQuery
    $ QueryPagedGet QueryTagProject mempty
    $ toUrlParts $ ("projects" :: Text)

projectsWithType :: MonadPlanMillQuery m => ViewTemplate -> m Projects
projectsWithType viewtemplate = planmillVectorQuery
    $ QueryPagedGet QueryTagProject (Map.fromList [("viewtemplate", fromString $ show $ viewTemplateToInt viewtemplate)])
    $ toUrlParts $ ("projects" :: Text)

-- | Get a list of tasks.
--
-- See <https://developers.planmill.com/api/#projects__project_id__tasks_get>
projectTasks :: MonadPlanMillQuery m => ProjectId -> m Tasks
projectTasks pid = planmillVectorQuery
    $ QueryPagedGet QueryTagTask mempty
    $ toUrlParts $ ("projects" :: Text) // pid // ("tasks" :: Text)

-- | View details of single task.
--
-- See <https://developers.planmill.com/api/#tasks__task_id__get>
--
-- TODO: seems to return 500 for most tasks
task :: MonadPlanMillQuery m => TaskId -> m Task
task tid = fmap (identifier .~ tid) -- this is a HACK, as not all task have id. We set it explicitly.
    $ planmillQuery
    $ QueryGet QueryTagTask mempty
    $ toUrlParts $ ("tasks" :: Text) // tid

-- | Get a list of capacitycalendars
--
-- See <http://developers.planmill.com/api/#capacitycalendars_get>
capacitycalendars :: MonadPlanMillQuery m => m CapacityCalendars
capacitycalendars = planmillVectorQuery
    $ QueryPagedGet QueryTagCalendar mempty
    $ toUrlParts $ ("capacitycalendars" :: Text)

-- | Get All Revenues 2 Report
--
-- See <https://developers.planmill.com/api/#reports__reportName__get>
allRevenuesReport :: (MonadPlanMillQuery m) => Integer -> Integer -> m AllRevenues2
allRevenuesReport year month = planmillQuery
    $ QueryGet QueryTagAllRevenue (Map.fromList [("param1",textShow year), ("param2", textShow month)])
    $ toUrlParts ("reports" :: Text) // ("All Revenues 2" :: Text)

-- | Get a Earned Vacations - report
--
-- See <https://developers.planmill.com/api/#reports__reportName__get>
earnedVacationsReport :: (MonadPlanMillQuery m) => Int -> m EarnedVacations
earnedVacationsReport organization = planmillVectorQuery
    $ QueryPagedGet QueryTagEarnedVacation qs
    $ toUrlParts $ ("reports" :: Text) // ("Earned Vacations" :: Text)
  where
    qs = Map.fromList
        [ ("param1", "-1") -- ^ Person
        , ("param2", "-1") -- ^ Team
        , ("param3", "-1") -- ^ Cost center
        , ("param5", (textShow organization)) -- ^ Organization
        , ("param6", "-1") -- ^ Year
        , ("param7", "1")  -- ^ Status
--        , ("param8", "2020-12-31T14:05:15.953Z") -- ^ Endtime
        ]

-- | Get Value creation by month per employee Report
--
-- See <https://developers.planmill.com/api/#reports__reportName__get>
valueCreationByMonthReport :: (MonadPlanMillQuery m) => Integer -> m PersonValueCreations
valueCreationByMonthReport year = planmillVectorQuery
    $ QueryPagedGet QueryTagValueCreation (Map.fromList [("param1",textShow year)])
    $ toUrlParts ("reports" :: Text) // ("Value creation per month by employee" :: Text)

-- | Get the "Teams hours by category" report
--
-- See <https://developers.planmill.com/api/#reports__reportName__get>
teamsHoursByCategoryReport :: (MonadPlanMillQuery m) => Interval Day -> m TeamsHoursByCategory
teamsHoursByCategoryReport interval = planmillVectorQuery $ QueryPagedGet QueryTagTeamsHours qs $ toUrlParts ( "reports" :: Text) // ("Teams hours by category" :: Text)
  where
    qs = flip elimInterval interval $ \a b -> Map.fromList [("param4", fromString . showPlanmillUTCTime $ UTCTime a 0)
                                                           ,("param5", fromString . showPlanmillUTCTime $ UTCTime b 0)]

-- | Get a list of tasks.
--
-- See <https://developers.planmill.com/api/#projects__project_id__tasks_get>
projectMembers :: MonadPlanMillQuery m => ProjectId -> m ProjectMembers
projectMembers pid = planmillVectorQuery
    $ QueryPagedGet QueryTagProjectMember mempty
    $ toUrlParts $ ("projects" :: Text) // pid // ("members" :: Text)

-- | Get assignments for project
--
-- See <https://developers.planmill.com/api/#projects__project_id__assignments_get>
assignments :: MonadPlanMillQuery m => ProjectId -> m Assignments
assignments pid = planmillVectorQuery
    $ QueryPagedGet QueryTagAssignment mempty
    $ toUrlParts $ ("projects" :: Text) // pid // ("assignments" :: Text)


-- | Get a hashmap of all possible portfolios
--
-- Uses the project meta field to fetch the values
portfolios :: MonadPlanMillQuery m => m Portfolios
portfolios = getPortfolio <$> planmillQuery
    ( QueryGet QueryTagMeta mempty
    $ toUrlParts $ ("projects" :: Text) // ("meta" :: Text))

-- | Get the metadata about invoices
--
-- See <https://developers.planmill.com/api_docs/#invoices_get>
invoices :: MonadPlanMillQuery m => Maybe Month -> m InvoiceDatas
invoices mmonth = planmillVectorQuery $ QueryPagedGet QueryTagInvoiceData qs $ toUrlParts $ ("invoices" :: Text)
  where
    qs = Map.fromList [("cyear", monthString)]
    monthString =
      case mmonth of
        Just (Month year mName) -> (textShow $ fromEnum mName) <> "/" <> (textShow year)
        Nothing    -> ""
-------------------------------------------------------------------------------
-- Duplication from PlanMill.Enumerations
-------------------------------------------------------------------------------

class Typeable entity => HasMeta entity where
    metaPath :: Proxy entity -> UrlParts

instance HasMeta User where
    metaPath _ = t "users" // t "meta"
      where t = id :: Text -> Text

instance HasMeta Absence where
    metaPath _ = t "absences" // t "meta"
      where t = id :: Text -> Text

instance HasMeta Timereport where
    metaPath _ = t "timereports" // t "meta"
      where t = id :: Text -> Text

instance HasMeta Account where
    metaPath _ = t "accounts" // t "meta"
      where t = id :: Text -> Text

instance HasMeta Project where
    metaPath _ = t "projects" // t "meta"
      where t = id :: Text -> Text

meta :: MonadPlanMillQuery m => HasMeta entity => Proxy entity -> m Meta
meta p = planmillQuery
    $ QueryGet QueryTagMeta mempty
    $ metaPath p

reifyTextSymbol :: forall r. Text -> (forall n. KnownSymbol n => Proxy n -> r) -> r
reifyTextSymbol t = reifySymbol (t ^. from packed)
