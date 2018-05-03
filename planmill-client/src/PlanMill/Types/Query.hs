{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module PlanMill.Types.Query (
    -- * Types
    Query (..),
    QueryTypes,
    QueryTag (..),
    -- * Request
    queryToRequest,
    -- * Existentials
    -- | Types with some type parameter hidden.
    SomeQuery(..),
    SomeQueryTag (..),
    SomeResponse(..),
    -- * Reflection
    queryType,
    queryDict,
    ) where

import Data.Binary                      (Put)
import Data.Binary.Tagged               (StructuralInfo (..))
import Data.Constraint
import Data.GADT.Compare                (GEq (..), defaultEq)
import Data.Reflection                  (reifySymbol)
import Data.Singletons.Bool             (SBoolI, sboolEqRefl)
import Data.Swagger                     (NamedSchema (..), ToSchema (..))
import Data.Type.Equality
import Futurice.Aeson                   (withObjectDump)
import Futurice.Constraint.ForallSymbol (ForallFSymbol (..))
import Generics.SOP                     (All, hcmap, hcollapse, hcpure)
import GHC.TypeLits                     (KnownSymbol, sameSymbol, symbolVal)
import PlanMill.Internal.Prelude
import Prelude ()

import qualified Data.Aeson.Compat                    as Aeson
import qualified Data.Map                             as Map
import qualified Data.Text                            as T
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres

import PlanMill.Types.Absence          (Absence, Absences)
import PlanMill.Types.Account          (Account, Accounts)
import PlanMill.Types.CapacityCalendar (CapacityCalendar, CapacityCalendars)
import PlanMill.Types.Enumeration      (EnumDesc)
import PlanMill.Types.Identifier       (Identifier (..))
import PlanMill.Types.Me               (Me)
import PlanMill.Types.Meta             (Meta)
import PlanMill.Types.Project          (Project, Projects)
import PlanMill.Types.Request
       (PlanMill (..), QueryString, planMillGetQs, planMillPagedGetQs)
import PlanMill.Types.ResultInterval
       (IntervalType (..), ResultInterval (..), intervalToQueryString)
import PlanMill.Types.Task             (Task, Tasks)
import PlanMill.Types.TeamMember       (TeamMember, TeamMembers)
import PlanMill.Types.TimeBalance      (TimeBalance)
import PlanMill.Types.Timereport       (Timereport, Timereports)
import PlanMill.Types.UOffset          (showPlanmillUTCTime)
import PlanMill.Types.UrlPart          (UrlParts, (//))
import PlanMill.Types.User             (Team, Teams, User, UserId, Users)
import PlanMill.Types.UserCapacity     (UserCapacities)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Arbitrary queries are tagged by this values
data QueryTag f a where
    QueryTagMe          :: QueryTag I Me
    QueryTagMeta        :: QueryTag I Meta
    QueryTagTeam        :: QueryTag f Team -- can be I or Vector
    QueryTagUser        :: QueryTag f User -- can be I or Vector
    QueryTagTimebalance :: QueryTag I TimeBalance
    QueryTagTimereport  :: QueryTag I Timereport
    QueryTagTask        :: QueryTag f Task    -- can be I or Vector
    QueryTagProject     :: QueryTag f Project -- can be I or Vector
    QueryTagAbsence     :: QueryTag f Absence -- can be I or Vector
    QueryTagAccount     :: QueryTag f Account -- can be I or Vector
    QueryTagCalendar    :: QueryTag f CapacityCalendar -- can be I or Vector
    QueryTagTeamMember  :: QueryTag Vector TeamMember
    QueryTagEnumDesc    :: KnownSymbol enum => !(Proxy enum) -> QueryTag I (EnumDesc enum)

-- | Planmill query (i.e. read-only operation).
--
-- More sophisticated then 'PlanMill' -request,
-- as it can be de/serialised.
data Query a where
    QueryGet         :: QueryTag I a -> QueryString -> UrlParts -> Query a
    QueryPagedGet    :: QueryTag Vector a -> QueryString -> UrlParts -> Query (Vector a)
    QueryTimereports :: Maybe (Interval Day) -> UserId -> Query Timereports
    QueryCapacities  :: Interval Day -> UserId -> Query UserCapacities
  deriving (Typeable)

-- | Transform Planmill query to request
queryToRequest :: Query a -> PlanMill a
queryToRequest (QueryGet _ qs ps)      = PlanMillGet qs ps
queryToRequest (QueryPagedGet _ qs ps) = PlanMillPagedGet  qs ps
queryToRequest (QueryTimereports i (Ident u)) =
    planMillPagedGetQs (qs <> qs') ("timereports" :: Text)
  where
    qs :: QueryString
    qs = maybe mempty dayIntervalToQueryString i
      where
        dayIntervalToQueryString
            = intervalToQueryString
            . ResultInterval IntervalStart

    qs' :: QueryString
    qs' = Map.fromList [ ("person", T.pack $ show u)
          ]
queryToRequest (QueryCapacities i uid) =
    planMillGetQs qs $ ("users" :: Text) // uid // ("capacity" :: Text)
  where
    qs = Map.fromList
        [ ("start",  fromString . showPlanmillUTCTime $ UTCTime (inf i) 0)
        , ("finish", fromString . showPlanmillUTCTime $ UTCTime (sup i) 0)
        ]

-- | Existential 'Query'
--
-- We could use:
--
-- @
-- data Some f where Some :: f a -> Some f
-- @
--
-- But than we have problems with writing instances.
-- This special case should be good for fit for us.
data SomeQuery where SomeQuery :: Query a -> SomeQuery
deriving instance Show SomeQuery

-- Helper to make deserialisation of SomeQuery simpler
data SomeQueryTag f where SomeQueryTag :: QueryTag f a -> SomeQueryTag f
deriving instance Show (SomeQueryTag f)

-------------------------------------------------------------------------------
-- QueryTag instances
-------------------------------------------------------------------------------

-- All tags are equal to itself
instance Eq (QueryTag f a) where _ == _ = True
instance Ord (QueryTag f a) where compare _ _ = EQ

instance GEq (QueryTag f) where
    geq QueryTagMe QueryTagMe                      = Just Refl
    geq QueryTagMeta QueryTagMeta                  = Just Refl
    geq QueryTagTeam QueryTagTeam                  = Just Refl
    geq QueryTagUser QueryTagUser                  = Just Refl
    geq QueryTagTimebalance QueryTagTimebalance    = Just Refl
    geq QueryTagTimereport QueryTagTimereport      = Just Refl
    geq QueryTagTask QueryTagTask                  = Just Refl
    geq QueryTagProject QueryTagProject            = Just Refl
    geq QueryTagAbsence QueryTagAbsence            = Just Refl
    geq QueryTagAccount QueryTagAccount            = Just Refl
    geq QueryTagCalendar QueryTagCalendar          = Just Refl
    geq QueryTagTeamMember QueryTagTeamMember      = Just Refl
    geq (QueryTagEnumDesc p) (QueryTagEnumDesc p') = do
        Refl <- sameSymbol p p'
        pure Refl
    geq _ _                                   = Nothing

instance Eq (SomeQueryTag f) where
    SomeQueryTag t == SomeQueryTag t' = defaultEq t t'

instance Show (QueryTag f a) where
    showsPrec d r = case r of
        QueryTagMe          -> showString "QueryTagMe"
        QueryTagMeta        -> showString "QueryTagMeta"
        QueryTagTeam        -> showString "QueryTagTeam"
        QueryTagUser        -> showString "QueryTagUser"
        QueryTagTimebalance -> showString "QueryTagTimebalance"
        QueryTagTimereport  -> showString "QueryTagTimereport"
        QueryTagTask        -> showString "QueryTagTask"
        QueryTagProject     -> showString "QueryTagProject"
        QueryTagAbsence     -> showString "QueryTagAbsence"
        QueryTagAccount     -> showString "QueryTagAccount"
        QueryTagCalendar    -> showString "QueryTagCalendar"
        QueryTagTeamMember  -> showString "QueryTagTeamMember"
        QueryTagEnumDesc p  -> showParen (d > 10)
            $ showString "QueryTagEnumDesc "
            . showsPrec 11 (symbolVal p)

instance Hashable (QueryTag f a) where
    hashWithSalt salt QueryTagMe           = salt `hashWithSalt` (0 :: Int)
    hashWithSalt salt QueryTagMeta         = salt `hashWithSalt` (1 :: Int)
    hashWithSalt salt QueryTagTeam         = salt `hashWithSalt` (2 :: Int)
    hashWithSalt salt QueryTagUser         = salt `hashWithSalt` (3 :: Int)
    hashWithSalt salt QueryTagTimebalance  = salt `hashWithSalt` (4 :: Int)
    hashWithSalt salt QueryTagTimereport   = salt `hashWithSalt` (5 :: Int)
    hashWithSalt salt QueryTagTask         = salt `hashWithSalt` (6 :: Int)
    hashWithSalt salt QueryTagProject      = salt `hashWithSalt` (7 :: Int)
    hashWithSalt salt QueryTagAbsence      = salt `hashWithSalt` (8 :: Int)
    hashWithSalt salt QueryTagAccount      = salt `hashWithSalt` (9 :: Int)
    hashWithSalt salt QueryTagCalendar     = salt `hashWithSalt` (10 :: Int)
    hashWithSalt salt QueryTagTeamMember   = salt `hashWithSalt` (11 :: Int)
    hashWithSalt salt (QueryTagEnumDesc p) = salt
        -- We don't add a int prehash, as it's unnecessary
        `hashWithSalt` (symbolVal p)

instance NFData (QueryTag f a) where
    rnf x = x `seq` ()

-- We use SBoolI, though we could have defined two instances:
  --
-- * @'Binary' ('SomeQueryTag' 'I')@
  --
-- * @'Binary' ('SomeQueryTag' 'Vector')@
  --
instance (SBoolI (f == I), SBoolI (f == Vector)) => Binary (SomeQueryTag f) where
    put (SomeQueryTag (QueryTagEnumDesc p)) = do
        put (0 :: Word8)
        put (symbolVal p)
    put (SomeQueryTag QueryTagMe)           = put (1 :: Word8)
    put (SomeQueryTag QueryTagMeta)         = put (2 :: Word8)
    put (SomeQueryTag QueryTagTeam)         = put (3 :: Word8)
    put (SomeQueryTag QueryTagUser)         = put (4 :: Word8)
    put (SomeQueryTag QueryTagTimebalance)  = put (5 :: Word8)
    put (SomeQueryTag QueryTagTimereport)   = put (6 :: Word8)
    put (SomeQueryTag QueryTagTask)         = put (7 :: Word8)
    put (SomeQueryTag QueryTagProject)      = put (8 :: Word8)
    put (SomeQueryTag QueryTagAbsence)      = put (9 :: Word8)
    put (SomeQueryTag QueryTagAccount)      = put (10 :: Word8)
    put (SomeQueryTag QueryTagCalendar)     = put (11 :: Word8)
    put (SomeQueryTag QueryTagTeamMember)   = put (12 :: Word8)

    get = get >>= \n -> case (n :: Word8, sboolEqRefl :: Maybe (f :~: I), sboolEqRefl :: Maybe (f :~: Vector)) of
        (0, Just Refl, _)  -> do
            val <- get
            pure $ reifySymbol val $ \p -> SomeQueryTag (QueryTagEnumDesc p)

        (1, Just Refl, _)  -> pure $ SomeQueryTag QueryTagMe
        (2, Just Refl, _)  -> pure $ SomeQueryTag QueryTagMeta
        (3, _, _)          -> pure $ SomeQueryTag QueryTagTeam
        (4, _, _)          -> pure $ SomeQueryTag QueryTagUser
        (5, Just Refl, _)  -> pure $ SomeQueryTag QueryTagTimebalance
        (6, Just Refl, _)  -> pure $ SomeQueryTag QueryTagTimereport
        (7, _, _)          -> pure $ SomeQueryTag QueryTagTask
        (8, _, _)          -> pure $ SomeQueryTag QueryTagProject
        (9, _, _)          -> pure $ SomeQueryTag QueryTagAbsence
        (10, _, _)         -> pure $ SomeQueryTag QueryTagAccount
        (11, _, _)         -> pure $ SomeQueryTag QueryTagCalendar
        (12, _, Just Refl) -> pure $ SomeQueryTag QueryTagTeamMember

        _ -> fail $ "Invalid tag " ++ show n

instance ToJSON (QueryTag f a) where
    toJSON QueryTagMe           = String "me"
    toJSON QueryTagMeta         = String "meta"
    toJSON QueryTagTeam         = String "team"
    toJSON QueryTagUser         = String "user"
    toJSON QueryTagTimebalance  = String "timebalance"
    toJSON QueryTagTimereport   = String "timereport"
    toJSON QueryTagTask         = String "task"
    toJSON QueryTagProject      = String "project"
    toJSON QueryTagAbsence      = String "absence"
    toJSON QueryTagAccount      = String "account"
    toJSON QueryTagCalendar     = String "calendar"
    toJSON QueryTagTeamMember   = String "teammember"
    toJSON (QueryTagEnumDesc p) = String $ "enumdesc-" <> symbolVal p ^. packed

instance ToJSON (SomeQueryTag f) where
    toJSON (SomeQueryTag t) = toJSON t

instance (SBoolI (f == I), SBoolI (f == Vector)) => FromJSON (SomeQueryTag f) where
    parseJSON = withText "SomeQueryTag" $ \t ->
        case (t, sboolEqRefl :: Maybe (f :~: I), sboolEqRefl :: Maybe (f :~: Vector)) of
            ("me",   Just Refl, _)        -> pure $ SomeQueryTag QueryTagMe
            ("meta", Just Refl, _)        -> pure $ SomeQueryTag QueryTagMeta
            ("team", _, _)                -> pure $ SomeQueryTag QueryTagTeam
            ("user", _, _)                -> pure $ SomeQueryTag QueryTagUser
            ("timebalance", Just Refl, _) -> pure $ SomeQueryTag QueryTagTimebalance
            ("timereport", Just Refl, _)  -> pure $ SomeQueryTag QueryTagTimereport
            ("task", _, _)                -> pure $ SomeQueryTag QueryTagTask
            ("project", _, _)             -> pure $ SomeQueryTag QueryTagProject
            ("absence", _, _)             -> pure $ SomeQueryTag QueryTagAbsence
            ("account", _, _)             -> pure $ SomeQueryTag QueryTagAccount
            ("calendar", _, _)            -> pure $ SomeQueryTag QueryTagCalendar
            ("teammember", _, Just Refl)  -> pure $ SomeQueryTag QueryTagTeamMember
            (_, Just Refl, _) | T.isPrefixOf pfx t
                -> pure $ reifySymbol (T.drop (T.length pfx) t ^. unpacked) mk
              where
                pfx = "enumdesc-"
                mk p = SomeQueryTag (QueryTagEnumDesc p)
            _ -> fail $ "Invalid tag: " ++ show t

instance HasStructuralInfo (QueryTag f a) where
    structuralInfo _ = StructuralInfo "QueryTag"
        [[ NominalType "QueryTagEnumDesc"
        ,  NominalType "QueryTagMe"
        ,  NominalType "QueryTagMeta"
        ,  NominalType "QueryTagTeam"
        ,  NominalType "QueryTagUser"
        ,  NominalType "QueryTagTimebalance"
        ,  NominalType "QueryTagTimereport"
        ,  NominalType "QueryTagTask"
        ,  NominalType "QueryTagProject"
        ,  NominalType "QueryTagAbsence"
        ,  NominalType "QueryTagAccount"
        ,  NominalType "QueryTagCalendar"
        ,  NominalType "QueryTagTeamMember"
        ]]

-------------------------------------------------------------------------------
-- Query instances
-------------------------------------------------------------------------------

deriving instance Eq (Query a)
-- deriving instance Ord (Query a)

instance GEq Query where
    geq (QueryGet t q p) (QueryGet t' q' p') = do
        Refl <- geq t t'
        guard (q == q' && p == p')
        pure Refl
    geq (QueryPagedGet t q p) (QueryPagedGet t' q' p') = do
        Refl <- geq t t'
        guard (q == q' && p == p')
        pure Refl
    geq (QueryTimereports i u) (QueryTimereports i' u')
        | liftEqMaybe eqInterval i i' && u == u' = Just Refl
    geq (QueryCapacities i u) (QueryCapacities i' u')
        | eqInterval i i' && u == u' = Just Refl
    geq _ _ = Nothing

instance Show (Query a) where
    showsPrec d r = showParen (d > 10) $ case r of
        QueryGet t q p -> showString "QueryGet "
            . showsPrec 11 t . showString " "
            . showsPrec 11 q . showString " "
            . showsPrec 11 p
        QueryPagedGet t q p -> showString "QueryPagedGet "
            . showsPrec 11 t . showString " "
            . showsPrec 11 q . showString " "
            . showsPrec 11 p
        QueryTimereports i u -> showString "QueryTimereports "
            . showsPrec 11 i . showString " "
            . showsPrec 11 u
        QueryCapacities i u -> showString "QueryCapacities "
            . showsPrec 11 i . showString " "
            . showsPrec 11 u

instance Hashable (Query a) where
    hashWithSalt salt (QueryGet t q p) = salt
        `hashWithSalt` (0 :: Int)
        `hashWithSalt` t
        `hashWithSalt` q
        `hashWithSalt` p
    hashWithSalt salt (QueryPagedGet t q p) = salt
        `hashWithSalt` (1 :: Int)
        `hashWithSalt` t
        `hashWithSalt` q
        `hashWithSalt` p
    hashWithSalt salt (QueryTimereports i u) = salt
        `hashWithSalt` (2 :: Int)
        `hashWithSalt` i
        `hashWithSalt` u
    hashWithSalt salt (QueryCapacities i u) = salt
        `hashWithSalt` (3 :: Int)
        `hashWithSalt` i
        `hashWithSalt` u

instance NFData (Query a) where
    rnf (QueryGet t q p)       = rnf t `seq` rnf q `seq` rnf p
    rnf (QueryPagedGet t q p)  = rnf t `seq` rnf q `seq` rnf p
    rnf (QueryTimereports i u) = rnf i `seq` rnf u
    rnf (QueryCapacities i u)  = rnf i `seq` rnf u

instance ToJSON (Query a) where
    toJSON (QueryGet t q p) = object
        [ "tag"   .= ("get" :: Text)
        , "type"  .= t
        , "query" .= q
        , "path"  .= p
        ]
    toJSON (QueryPagedGet t q p) = object
        [ "tag"   .= ("paged-get" :: Text)
        , "type"  .= t
        , "query" .= q
        , "path"  .= p
        ]
    toJSON (QueryTimereports i u) = object
        [ "tag"      .= ("timereports" :: Text)
        , "interval" .= i
        , "uid"      .= u
        ]
    toJSON (QueryCapacities i u) = object
        [ "tag"      .= ("capacities" :: Text)
        , "interval" .= i
        , "uid"      .= u
        ]

-- | Encoded as JSON
instance Postgres.ToField (Query a) where
    toField = Postgres.toField . Aeson.encode

-- Unfortunately we cannot autoderive this
instance HasStructuralInfo (Query a) where
    structuralInfo _ = StructuralInfo "SomeQuery"
        [ [ structuralInfo (Proxy :: Proxy :$ QueryTag I ())
          , structuralInfo (Proxy :: Proxy QueryString)
          , structuralInfo (Proxy :: Proxy UrlParts)
          ]
        , [ structuralInfo (Proxy :: Proxy :$ QueryTag Vector ())
          , structuralInfo (Proxy :: Proxy QueryString)
          , structuralInfo (Proxy :: Proxy UrlParts)
          ]
        , [ structuralInfo (Proxy :: Proxy :$ Maybe :$ Interval Day)
          , structuralInfo (Proxy :: Proxy UserId)
          ]
        , [ structuralInfo (Proxy :: Proxy :$ Interval Day)
          , structuralInfo (Proxy :: Proxy UserId)
          ]
        ]

-------------------------------------------------------------------------------
-- QueryTypes
-------------------------------------------------------------------------------

-- | Possible 'Query' types
type QueryTypes = '[ Timereports, UserCapacities
    , Me
    , User, Users
    , Meta
    , Team, Teams
    , TimeBalance
    , Timereport
    , Project, Projects
    , Task, Tasks
    , Absence, Absences
    , Account, Accounts
    , CapacityCalendar, CapacityCalendars
    , TeamMembers
    ]

-- | A bit fancier than ':~:'
data IsSomeEnumDesc a where
    IsSomeEnumDesc  :: KnownSymbol k => Proxy k -> IsSomeEnumDesc (EnumDesc k)

queryTagType
    :: QueryTag I a
    -> Either (IsSomeEnumDesc a) (NS ((:~:) a) QueryTypes)
queryTagType QueryTagMe           = Right $ insertNS Refl
queryTagType QueryTagUser         = Right $ insertNS Refl
queryTagType QueryTagMeta         = Right $ insertNS Refl
queryTagType QueryTagTeam         = Right $ insertNS Refl
queryTagType QueryTagTimebalance  = Right $ insertNS Refl
queryTagType QueryTagTimereport   = Right $ insertNS Refl
queryTagType QueryTagTask         = Right $ insertNS Refl
queryTagType QueryTagProject      = Right $ insertNS Refl
queryTagType QueryTagAbsence      = Right $ insertNS Refl
queryTagType QueryTagAccount      = Right $ insertNS Refl
queryTagType QueryTagCalendar     = Right $ insertNS Refl
queryTagType (QueryTagEnumDesc p) = Left $ IsSomeEnumDesc p

queryTagVectorType
    :: QueryTag Vector a
    -> NS ((:~:) (Vector a)) QueryTypes
queryTagVectorType QueryTagUser       = insertNS Refl
queryTagVectorType QueryTagTeam       = insertNS Refl
queryTagVectorType QueryTagTask       = insertNS Refl
queryTagVectorType QueryTagProject    = insertNS Refl
queryTagVectorType QueryTagAbsence    = insertNS Refl
queryTagVectorType QueryTagAccount    = insertNS Refl
queryTagVectorType QueryTagCalendar   = insertNS Refl
queryTagVectorType QueryTagTeamMember = insertNS Refl
#if __GLASGOW_HASKELL__ < 800
queryTagVectorType _ = error "queryTagVectorType: panic!"
#endif

-- | Reflect the type of 'Query'.
queryType
    :: Query a
    -> Either (IsSomeEnumDesc a) (NS ((:~:) a) QueryTypes)
queryType (QueryGet t _ _)       = queryTagType t
queryType (QueryPagedGet t _ _)  = Right $ queryTagVectorType t
queryType (QueryTimereports _ _) = Right $ Z Refl
queryType (QueryCapacities _ _)  = Right $ S (Z Refl)

-- This is a helper class to help writing above proofs.
--
-- We can write them by hand, but it's tedious
class InsertNS x ys where
    insertNS :: f x -> NS f ys

-- | We don't care about which proof we get.
-- i.e. the above proofs are irrelevant.
instance {-# INCOHERENT #-} InsertNS x (x ': ys) where
    insertNS = Z

instance InsertNS x ys => InsertNS x (y ': ys) where
    insertNS = S . insertNS

-------------------------------------------------------------------------------
-- SomeQuery instances
-------------------------------------------------------------------------------

instance Eq SomeQuery where
    SomeQuery q == SomeQuery q' = defaultEq q q'

-- | Note: not sure this is lawful, or consistent with 'Eq' instance
instance Ord SomeQuery where
    SomeQuery q `compare` SomeQuery q' = Aeson.encode q `compare` Aeson.encode q'

instance Hashable SomeQuery where
    hashWithSalt salt (SomeQuery q) = hashWithSalt salt q

instance ToJSON SomeQuery where
    toJSON (SomeQuery q) = toJSON q

instance ToSchema SomeQuery where
    declareNamedSchema _ = pure $ NamedSchema (Just "PlanMill Query") mempty

instance FromJSON SomeQuery where
    parseJSON = withObjectDump "PM.Query" $ \obj -> do
        tag <- obj .: "tag"
        case (tag :: Text) of
            "get" -> mkSomeQueryGet
                <$> obj .: "type"
                <*> obj .: "query"
                <*> obj .: "path"
            "paged-get" -> mkSomeQueryPagedGet
                <$> obj .: "type"
                <*> obj .: "query"
                <*> obj .: "path"
            "timereports" -> mkSomeQueryTimereports
                <$> obj .: "interval"
                <*> obj .: "uid"
            "capacities" -> mkSomeQueryCapacities
                <$> obj .: "interval"
                <*> obj .: "uid"
            _ -> fail $ "Invalid query tag: " ++ show tag

instance Binary SomeQuery where
    put (SomeQuery (QueryGet t q p)) = do
        put (0 :: Word8)
        put (SomeQueryTag t)
        put q
        put p
    put (SomeQuery (QueryPagedGet t q p)) = do
        put (1 :: Word8)
        put (SomeQueryTag t)
        put q
        put p
    put (SomeQuery (QueryTimereports i u)) = do
        put (2 :: Word8)
        put i
        put u
    put (SomeQuery (QueryCapacities i u)) = do
        put (3 :: Word8)
        put i
        put u

    get = get >>= \n -> case n :: Word8 of
        0 -> mkSomeQueryGet <$> get <*> get <*> get
        1 -> mkSomeQueryPagedGet <$> get <*> get <*> get
        2 -> mkSomeQueryTimereports <$> get <*> get
        3 -> mkSomeQueryCapacities <$> get <*> get
        _ -> fail $ "Invalid tag: " ++ show n

mkSomeQueryGet :: SomeQueryTag I-> QueryString -> UrlParts -> SomeQuery
mkSomeQueryGet t q p = case t of
    SomeQueryTag t' -> SomeQuery (QueryGet t' q p)

mkSomeQueryPagedGet :: SomeQueryTag Vector -> QueryString -> UrlParts -> SomeQuery
mkSomeQueryPagedGet t q p = case t of
    SomeQueryTag t' -> SomeQuery (QueryPagedGet t' q p)

mkSomeQueryTimereports :: Maybe (Interval Day) -> UserId -> SomeQuery
mkSomeQueryTimereports i u = SomeQuery (QueryTimereports i u)

mkSomeQueryCapacities :: Interval Day -> UserId -> SomeQuery
mkSomeQueryCapacities i u = SomeQuery (QueryCapacities i u)

-- | Encoded as JSON
instance Postgres.ToField SomeQuery where
    toField (SomeQuery q) = Postgres.toField q

instance Postgres.FromField SomeQuery where
    fromField f mbs = do
        bs <- Postgres.fromField f mbs
        case Aeson.eitherDecode bs of
            Right x  -> pure x
            Left err -> Postgres.conversionError (Aeson.AesonException err)

-------------------------------------------------------------------------------
-- Query dictionaries
-------------------------------------------------------------------------------

-- | We can recover all type parameters 'Query' can has.
queryDict
    :: forall c a. (All c QueryTypes, ForallFSymbol c EnumDesc)
    => Proxy c -> Query a -> Dict (c a)
queryDict pc q = case queryType q of
    Right ns                 -> f ns
    Left (IsSomeEnumDesc _)  -> instFSymbol
  where
    f :: NS ((:~:) a) QueryTypes -> Dict (c a)
    f xs = hcollapse (hcmap pc (\Refl -> K Dict) xs)

-------------------------------------------------------------------------------
-- SomeResponse
-------------------------------------------------------------------------------

-- | An existential type bundling the 'Query' and it's response.
--
-- This can be serialised and deserialised, which is great!
data SomeResponse where
    MkSomeResponse :: Query a -> a -> SomeResponse

instance Eq SomeResponse where
    MkSomeResponse q x == MkSomeResponse q' x' = fromMaybe False $ do
        Refl <- geq q q'
        case queryDict (Proxy :: Proxy Eq) q of
            Dict -> return (x == x')

-- | Note: Use this instance only for debugging!
instance Show SomeResponse where
    showsPrec d (MkSomeResponse q r) = case queryDict (Proxy :: Proxy Show) q of
        Dict ->  showParen (d > 10)
            $ showString "MkSomeResponse "
            . showsPrec 11 q
            . showString " "
            . showsPrec 11 r

instance Binary SomeResponse where
    put (MkSomeResponse q r) = put' q r
      where
        put' :: forall a. Query a -> a -> Put
        put' q' r' = case queryDict (Proxy :: Proxy Binary) q' of
            Dict -> put (SomeQuery q') >> put r'

    get = do
        SomeQuery q <- get
        case queryDict (Proxy :: Proxy Binary)  q of
            Dict -> do
                r <- get
                pure (MkSomeResponse q r)

instance NFData SomeResponse where
    rnf (MkSomeResponse q a) = case queryDict (Proxy :: Proxy NFData) q of
        Dict -> rnf q `seq` rnf a

instance HasSemanticVersion SomeResponse

instance HasStructuralInfo SomeResponse where
    structuralInfo _ =
        StructuralInfo "SomeResponse" [ queryInfo : responseInfo ]
      where
        queryInfo = structuralInfo (Proxy :: Proxy (Query ()))
        responseInfo = hcollapse infos

        infos :: NP (K StructuralInfo) QueryTypes
        infos = hcpure (Proxy :: Proxy HasStructuralInfo) f

        f :: forall a. HasStructuralInfo a => K StructuralInfo a
        f = K $ structuralInfo (Proxy :: Proxy a)

instance ToSchema SomeResponse where
    declareNamedSchema _ = pure $ NamedSchema (Just "Some planmill query response") mempty

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

eqInterval :: Eq a => Interval a -> Interval a -> Bool
eqInterval i j = sup i == sup j && inf i == inf j

-- TODO: require transformers-0.5
liftEqMaybe :: (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
liftEqMaybe _eq Nothing  Nothing  = True
liftEqMaybe  eq (Just a) (Just b) = eq a b
liftEqMaybe _eq _        _        = False
