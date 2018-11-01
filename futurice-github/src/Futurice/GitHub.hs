{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.GitHub (
    -- * Tags
    ReqTag,
    mkReqTag,
    typeTagDict,
    -- * Types
    GHTypes,
    -- * Request / Response
    SomeRequest (..),
    SomeResponse (..),
    -- * Re-exports
    module GitHub,
    -- * Internals
    requestToJSON,
    ) where

import Prelude ()

import Control.Lens       (review)
import Data.Aeson.Compat
       (FromJSON (..), Object, ToJSON (..), object, (.:), (.=))
import Data.Aeson.Types   (Pair, Parser)
import Data.Binary.Tagged
       (HasSemanticVersion, HasStructuralInfo (..), StructuralInfo (..))
import Data.Constraint    (Dict (..))
import Data.GADT.Compare  (GOrdering (..), gcompare, geq)
import Data.Swagger       (NamedSchema (..), ToSchema (..))
import Data.Type.Equality
import Futurice.Aeson     (withObjectDump)
import Futurice.Has       (In, inj)
import Futurice.List      (Append, TMap, splitAppend, tmapToNSComp)
import Futurice.Prelude   hiding (Pair)
import Futurice.TypeTag
import Generics.SOP       ((:.:) (..), SListI, hcollapse, hcpure)

import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres

import GitHub

-------------------------------------------------------------------------------
-- Enumeration
-------------------------------------------------------------------------------

-- | Types of requests that can be serialised.
type GHTypes = Append Scalars (TMap Vector Collections)
type Scalars =
    '[ Organization
    , Owner
    , Repo
    , Team
    , User
    ]

type Collections =
   '[ Event
    , Issue
    , Repo
    , SimpleOrganization
    , SimpleTeam
    , SimpleUser
    , Invitation
    ]

-------------------------------------------------------------------------------
-- Tag
-------------------------------------------------------------------------------

type ReqTag a = TT GHTypes a

-- TODO: move to futurice-prelude
mkReqTag :: In a GHTypes => ReqTag a
mkReqTag = TT (review inj Refl)

-------------------------------------------------------------------------------
-- Request
-------------------------------------------------------------------------------

-- | Existential request.
data SomeRequest where
    MkSomeRequest :: ReqTag a -> Request 'RA a -> SomeRequest

instance Eq SomeRequest where
    MkSomeRequest t r == MkSomeRequest t' r' = fromMaybe False $ do
        Refl <- geq t t'
        case typeTagDict (Proxy :: Proxy Eq) t of
            Dict -> pure (r == r')

instance Ord SomeRequest where
    MkSomeRequest t r `compare` MkSomeRequest t' r' =
        case gcompare t t' of
            GLT -> LT
            GGT -> GT
            GEQ -> case typeTagDict (Proxy :: Proxy Ord) t of
                Dict -> compare r r'

instance Hashable SomeRequest where
    hashWithSalt salt (MkSomeRequest t r) = salt
        `hashWithSalt` (SomeTT t)
        `hashWithSalt` r

instance Show SomeRequest where
    showsPrec d (MkSomeRequest t r) = showParen (d > 10)
        $ showString "MkSomeRequest "
        . showsPrec 11 t
        . showChar ' '
        . showsPrec 11 r

instance ToJSON SomeRequest where
    toJSON (MkSomeRequest t r) = object $
        [ "tag" .= SomeTT t
        ] ++ requestToJSON r

instance Postgres.ToField SomeRequest where
    toField = Postgres.toJSONField

instance Postgres.FromField SomeRequest where
    fromField = Postgres.fromJSONField

instance ToSchema SomeRequest where
    declareNamedSchema _ = pure $ NamedSchema (Just "Some github request") mempty

requestToJSON :: Request 'RA a -> [Pair]
requestToJSON (SimpleQuery q) = simpleRequestToJSON q
requestToJSON _               = []

simpleRequestToJSON :: SimpleRequest 'RA a -> [Pair]
simpleRequestToJSON (Query ps qs) =
    [ "type"  .= ("query" :: Text)
    , "paths" .= ps
    , "query" .= queryStringToText qs
    ]
simpleRequestToJSON (PagedQuery ps qs fc) =
    [ "type"  .= ("pagedquery" :: Text)
    , "paths" .= ps
    , "query" .= queryStringToText qs
    , "count" .= fetchCountToJSON fc
    ]

instance FromJSON SomeRequest where
    parseJSON = withObjectDump "GH.SomeRequest" $ \obj -> do
        SomeTT tag <- obj .: "tag"
        typ <- obj .: "type" :: Parser Text
        case typeTagDict (Proxy :: Proxy FromJSON) tag of
            Dict -> case typ of
                "query" -> do
                    req <- queryParseJSON obj
                    pure $ MkSomeRequest tag (SimpleQuery req)
                "pagedquery" -> case ghtypeIsVector tag of
                    Nothing -> fail $ "Tag is not of vector type for paged req"
                    Just MkIsVector -> do
                        req <- pagedQueryParseJSON obj
                        pure $ MkSomeRequest tag (SimpleQuery req)
                _ -> fail $ "Unknown query type: " ++ show typ

queryParseJSON :: Object -> Parser (SimpleRequest 'RA a)
queryParseJSON obj = mkQuery
    <$> obj .: "paths"
    <*> obj .: "query"
  where
    mkQuery :: [Text] -> [(Text, Text)] -> SimpleRequest 'RA a
    mkQuery ps qs = Query ps (textToQueryString qs)

pagedQueryParseJSON :: Object -> Parser (SimpleRequest 'RA (Vector a))
pagedQueryParseJSON obj = mkPagedQuery
    <$> obj .: "paths"
    <*> obj .: "query"
    <*> obj .: "count"
  where
    mkPagedQuery :: [Text] -> [(Text, Text)] -> Maybe Word -> SimpleRequest 'RA (Vector a)
    mkPagedQuery ps qs fc = PagedQuery ps (textToQueryString qs) (fetchCountParseJSON fc)

-------------------------------------------------------------------------------
-- SomeResponse
-------------------------------------------------------------------------------

-- | Existential response.
data SomeResponse where
    MkSomeResponse :: ReqTag a -> a -> SomeResponse

instance NFData SomeResponse where
    rnf (MkSomeResponse t x) = do
        case typeTagDict (Proxy :: Proxy NFData) t of
            Dict -> rnf x

instance Binary SomeResponse where
    put (MkSomeResponse t x) = do
        put (SomeTT t)
        case typeTagDict (Proxy :: Proxy Binary) t of
            Dict -> put x

    get = do
        SomeTT t <- get
        case typeTagDict (Proxy :: Proxy Binary) t of
            Dict -> do
                x <- get
                pure $ MkSomeResponse t x

instance HasSemanticVersion SomeResponse

instance HasStructuralInfo SomeResponse where
    structuralInfo _ = StructuralInfo "GitHub.SomeResponse" [responseInfo]
      where
        responseInfo = hcollapse infos

        infos :: NP (K StructuralInfo) GHTypes
        infos = hcpure (Proxy :: Proxy HasStructuralInfo) f

        f :: forall a. HasStructuralInfo a => K StructuralInfo a
        f = K $ structuralInfo (Proxy :: Proxy a)

instance ToSchema SomeResponse where
    declareNamedSchema _ = pure $ NamedSchema (Just "Some github response") mempty

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

queryStringToText :: QueryString -> [(Text, Text)]
queryStringToText = mapMaybe f
  where
    f (k, v) = (,) (decodeUtf8Lenient k) . decodeUtf8Lenient <$> v

textToQueryString :: [(Text, Text)] -> QueryString
textToQueryString = fmap (bimap encodeUtf8 (Just . encodeUtf8))

fetchCountToJSON :: FetchCount -> Maybe Word
fetchCountToJSON (FetchAtLeast n) = Just n
fetchCountToJSON FetchAll         = Nothing

fetchCountParseJSON :: Maybe Word -> FetchCount
fetchCountParseJSON Nothing  = FetchAll
fetchCountParseJSON (Just n) = FetchAtLeast n

data IsVector a where
    MkIsVector :: IsVector (Vector b)

ghtypeIsVector :: ReqTag a -> Maybe (IsVector a)
ghtypeIsVector (TT t) = repIsVector'
    (Proxy :: Proxy Scalars)
    (Proxy :: Proxy Collections)
    t

repIsVector'
    :: forall xs ys a. (SListI xs, SListI ys)
    => Proxy xs -> Proxy ys
    -> NS (Is a) (Append xs (TMap Vector ys))
    -> Maybe (IsVector a)
repIsVector' _pxs _pys ns =
    case splitAppend ns :: Either (NS (Is a) xs) (NS (Is a) (TMap Vector ys)) of
        Left _    -> Nothing
        Right ns' -> Just $ repIsVector (tmapToNSComp ns' :: NS (Is a :.: Vector) ys)

repIsVector :: NS (Is a :.: Vector) xs -> IsVector a
repIsVector (Z (Comp Refl)) = MkIsVector
repIsVector (S ns)          = repIsVector ns
