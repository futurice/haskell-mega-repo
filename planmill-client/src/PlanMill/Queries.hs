{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- | High-level query api
module PlanMill.Queries (
    -- * Monad class
    MonadPlanMillQuery,
    -- * Special queries
    timereports,
    allTimereports,
    capacities,
    enumerationValue,
    -- * Non special queries
    me,
    user,
    users,
    team,
    -- * Queries
    usersQuery,
    -- timereportsModifiedQuery,
    ) where

import PlanMill.Internal.Prelude

import Data.Constraint (Dict (..))
import Data.Reflection (reifySymbol)
import GHC.TypeLits    (KnownSymbol, symbolVal)

import Control.Monad.PlanMill

import Numeric.Interval.NonEmpty   (Interval)
import PlanMill.Types.Enumeration
import PlanMill.Types.Me           (Me)
import PlanMill.Types.Meta         (Meta, lookupFieldEnum)
import PlanMill.Types.Query        (Query (..), QueryTag (..))
import PlanMill.Types.Timereport   (Timereports)
import PlanMill.Types.UrlPart      (UrlParts, toUrlParts, (//))
import PlanMill.Types.User         (Team, TeamId, User, UserId, Users)
import PlanMill.Types.UserCapacity (UserCapacities)

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
        , MonadPlanMillQuery m
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

enumerationForField
    :: forall entity field m.
        ( HasMeta entity
        , KnownSymbol field
        , MonadPlanMillQuery m
        )
    => Proxy entity -> Proxy field
    -> m (Maybe SomeEnumDesc)
enumerationForField entityProxy fieldNameProxy = do
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

-------------------------------------------------------------------------------
-- Duplication from PlanMill.Enumerations
-------------------------------------------------------------------------------

class HasMeta entity where
    metaPath :: Proxy entity -> UrlParts

instance HasMeta User where
    metaPath _ = t "users" // t "meta"
      where t = id :: Text -> Text

meta :: MonadPlanMillQuery m => HasMeta entity => Proxy entity -> m Meta
meta p = planmillQuery
    $ QueryGet QueryTagMeta mempty
    $ metaPath p

reifyTextSymbol :: forall r. Text -> (forall n. KnownSymbol n => Proxy n -> r) -> r
reifyTextSymbol t = reifySymbol (t ^. from packed)
