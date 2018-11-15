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
    project,
    projects,
    projectTasks,
    task,
    capacitycalendars,
    -- * Queries
    usersQuery,
    absencesQuery,
    -- timereportsModifiedQuery,
    ) where

import PlanMill.Internal.Prelude

import Control.Lens          (alongside, withIndex)
import Control.Monad.Memoize (MonadMemoize (memo))
import Data.Constraint       (Dict (..))
import Data.Reflection       (reifySymbol)
import GHC.TypeLits          (KnownSymbol, symbolVal)

import Control.Monad.PlanMill
import PlanMill.Types
       (Absence, Absences, Account, AccountId, CapacityCalendars, Me, Project,
       ProjectId, Projects, Task, TaskId, Tasks, Team, TeamId, TimeBalance,
       Timereport, Timereports, User, UserCapacities, UserId, Users,
       identifier)
import PlanMill.Types.Enumeration
import PlanMill.Types.Meta        (Meta, lookupFieldEnum)
import PlanMill.Types.Query       (Query (..), QueryTag (..))
import PlanMill.Types.UrlPart     (UrlParts, toUrlParts, (//))

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
-- See <https://developers.planmill.com/api/#projects__project_id__get>
project :: MonadPlanMillQuery m => ProjectId -> m Project
project pid = planmillQuery
    $ QueryGet QueryTagProject mempty
    $ toUrlParts $ ("projects" :: Text) // pid

-- | Get a list of projects.
--
-- See <https://developers.planmill.com/api/#projects_get>
projects :: MonadPlanMillQuery m => m Projects
projects = planmillVectorQuery
    $ QueryPagedGet QueryTagProject mempty
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
