{-# LANGUAGE CPP                     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif
module Futurice.Report.Columns (
    -- * Report
    Report (..),
    ReportGenerated (..),
    reportParams,
    reportData,
    reportToTabularEncoding,
    -- * Cell
    ReportValue (..),
    -- * Class
    ToColumns (..),
    DefaultColumns,
    ) where

import Data.Aeson                (encode, pairs, (.=))
import Data.Aeson.Encoding       (encodingToLazyByteString, list, pair)
import Data.Constraint           (Constraint)
import Data.Swagger              (NamedSchema (..))
import Futurice.Constraint.Unit1 (Unit1)
import Futurice.Generics
import Futurice.Generics.SOP     (sopRecordFieldNames)
import Futurice.IsMaybe
import Futurice.List
import Futurice.Lucid.Foundation
import Futurice.Office           (Office)
import Futurice.Prelude
import Futurice.Time
       (AsScientific, IsTimeUnit (..), NDT (..), TimeUnit (..))
import Futurice.Tribe            (Tribe)
import Generics.SOP              ((:.:) (..), All, SListI (..))
import GHC.TypeLits              (KnownSymbol, Symbol, symbolVal)
import Prelude ()
import Servant.API               (MimeRender (..))
import Servant.CSV.Cassava       (CSV', EncodeOpts, SHasHeaderI, encodeOpts')

import qualified Data.Csv          as Csv
import qualified Data.Set          as Set
import qualified Data.Tuple.Strict as S
import qualified Generics.SOP      as SOP
import qualified PlanMill          as PM

-- instances
import qualified Chat.Flowdock.REST as FD
import qualified FUM.Types.Login    as FUM
import qualified GitHub             as GH
import qualified Personio

-------------------------------------------------------------------------------
-- Reader Html
-------------------------------------------------------------------------------

type ReaderHtml env = HtmlT ((->) env)

-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

-- | The report.
--
-- Report is parameterised over
--
-- * @name@ - the name
--
-- * @params@ - the report parameteters, e.g. 'ReportGenerated'
--
-- * @fs@ and @a@ - report actual data
--
data Report (name :: Symbol) params a = Report
    { _reportParams :: !params
    , _reportData   :: !a
    }
    deriving (Typeable, Functor, Foldable, Traversable)

makeLenses ''Report
deriveGeneric ''Report

instance (NFData params, NFData a) => NFData (Report name params a) where
    rnf (Report params a) = rnf params `seq` rnf a

-------------------------------------------------------------------------------
-- Report + aeson
-------------------------------------------------------------------------------

deriveVia [t| forall name params a. ((ToJSON a, ToJSON params, IsMaybe a, IsMaybe params) => ToJSON (Report name params a))       `Via` Sopica (Report name params a) |]
deriveVia [t| forall name params a. ((FromJSON a, FromJSON params, IsMaybe a, IsMaybe params) => FromJSON (Report name params a)) `Via` Sopica (Report name params a) |]

instance (ToSchema a, ToSchema params, KnownSymbol name)
    => ToSchema (Report name params a)
  where
    declareNamedSchema p = do
        NamedSchema _ schema <- sopDeclareNamedSchema p
        pure $ NamedSchema (Just $ title ^. packed) schema
      where
        title = "Report: " <> symbolVal (Proxy :: Proxy name)

-------------------------------------------------------------------------------
-- Report + cassava + servant
-------------------------------------------------------------------------------

instance
    ( ToColumns a
    , All Csv.ToField (Columns a)
    , EncodeOpts opt
    , SHasHeaderI hasHeader
    )
    => MimeRender (CSV' hasHeader opt) (Report name params a)
  where
    mimeRender _
        = Csv.encodeWith (encodeOpts' (Proxy :: Proxy opt) (Proxy :: Proxy hasHeader))
        . toColumns . _reportData

-------------------------------------------------------------------------------
-- Report + lucid
-------------------------------------------------------------------------------

class (ReportValue a, ReportValueC a env) => ReportValue' env a
instance (ReportValue a, ReportValueC a env) => ReportValue' env a

columnControl
    :: forall env a. ReportValue' env a
    => ColumnData a
    -> ReaderHtml env ()
columnControl (ColumnData colname xs) = largemed_ 6 $ div_ [ class_ "futu-report-control" ] $ do
    h3_ $ toHtml colname
    -- sort
    {- -- TODO: think how to present sorting controls
    div_ $ do
        button_ [ class_ "button futu-report-sort-asc" ] $ "Sort ascending"
        " "
        button_ [ class_ "button futu-report-sort-desc" ] $ "Sort descending"
    -}
    -- aggregate
    label_ $ do
        "Aggregate"
        select_ [ class_ "futu-report-aggregate" ] $ for_ (columnTypeAggregate colType) $ \agg -> do
            let (s, i, t) = aggregateMeta agg
            option_ [ value_ i ] $ toHtml $ s <> " : " <> t
    -- group
    div_ $ label_ $ do
        input_ [ type_ "checkbox", class_ "futu-report-group-by" ]
        " Group by"
    -- values
    when showValues $ label_ $ do
        "Filter values"
        select_ [ class_ "futu-report-filter", multiple_ "multiple" ] $
            for_ xs' $ \x->
                option_ [ value_ $ decodeUtf8Lenient $ encode x ^. strict ] $
                    reportValueHtml x
  where
    colType    = reportValueType (Proxy :: Proxy a)
    xs'        = Set.fromList $ xs
    showValues = length xs' < 20



instance
    ( ToColumns a
    , SOP.All ReportValue (Columns a)
    , SOP.All (ReportValue' params) (Columns a)
    , SOP.All ToJSON (Columns a) -- Redundant, but GHC isn't smart enough.
    , KnownSymbol name, ToHtml params, ToJSON params
    ) => ToHtml (Report name params a)
  where
    toHtmlRaw = toHtml

    toHtml :: forall m. Monad m => Report name params a -> HtmlT m ()
    toHtml report@(Report params d) = toHtml $ page_ (fromString title) pageParams $ do
        row_ $ large_ 12 $ h1_ $ fromString title
        row_ $ large_ 12 $ div_ [class_ "callout"] $ toHtml params
        hoist (\m -> return (m params)) $ div_ [class_ "futu-report-wrapper" ] $ do
            -- Data: hidden div with report data in tabular json format
            div_ [class_ "futu-report-data", style_ "display: none" ] $ toHtml $
                reportToTabularEncoding report
            -- Control: we generate them, initially invisible, if js fails for reason or another.
            row_ $ large_ 12 $ div_ [class_ "futu-report-controls callout" ] $ do
                button_ [ class_ "button futu-report-toggle" ] $ "Show controls"

                -- per column controls
                div_ [ class_ "futu-report-toggleable-controls" ] $ do
                    hr_ []
                    sequenceA_ $ SOP.hcollapse $
                        -- GHC doesn't believe this expression is well-typed
                        -- without very strong conveniencing
                        ((SOP.hcmap proxyReportValue) ::
                            ((forall x. ReportValue' params x => ColumnData x -> K (ReaderHtml params ()) x)
                                -> NP ColumnData (Columns a) -> NP (K (ReaderHtml params ())) (Columns a)))
                            (K . columnControl) colData

                -- Query string
                hr_ []
                pre_ [ class_ "futu-report-query-str" ] $ "SELECT *\nFROM report;"

                -- apply & reset
                div_  [ class_ "futu-report-toggleable-controls-2" ] $ do
                    hr_ []
                    div_ $ do
                        button_ [ class_ "button success futu-report-apply", disabled_ "disabled" ] $ "Apply settings"
                        " "
                        button_ [ class_ "button warning futu-report-reset"] $ "Reset controls"

            -- Pregenerated data, so we see something, even the data fails
            row_ $ large_ 12 $ table_ [class_ "futu-report"] $ do
                thead_ $ do
                    tr_ $ for_ colNames $ \(colName, _) -> th_ $ toHtml colName
                    -- Quick controls
                    tr_ [ class_ "futu-report-quick-controls" ] $ for_ colNames $ \(_colName, colType) -> td_ $ do
                        a_ [ href_ "#", data_ "futu-report-link-control" "sort-asc", title_ "sort ascending" ] "⇈"
                        " "
                        a_ [ href_ "#", data_ "futu-report-link-control" "sort-desc",title_ "sort descending" ] "⇊"
                        " | "
                        forWith_ " " (columnTypeAggregate colType) $ \agg -> do
                            let (s, i, t) = aggregateMeta agg
                            a_ [ href_ "#", title_ t, data_ "futu-report-link-control" i ] $
                                toHtml s
                        " | "
                        a_ [ href_ "#", data_ "futu-report-link-control" "group-by", title_ "group by this column" ] $ toHtmlRaw ("G" :: Text)
                tbody_ $ for_ columns $ \r -> tr_ {- todo: class -} $
                    sequenceA $ SOP.hcollapse $
                        -- See note above
                        ((SOP.hcmap proxyReportValue) ::
                            ((forall x. ReportValue' params x => I x -> K (ReaderHtml params ()) x)
                                -> NP I (Columns a) -> NP (K (ReaderHtml params ())) (Columns a)))
                        (K . renderCell . unI) r

      where
        -- Data
        title  :: String
        title   = symbolVal (Proxy :: Proxy name)

        names  :: NP (K Text) (Columns a)
        names   = columnNames (Proxy :: Proxy a)

        columns :: [NP I (Columns a)]
        columns = toColumns d

        -- Page parameters
        pageParams = defPageParams
            & pageJs .~ [ $(embedJS "report-columns.js") ]

        -- Proxies
        proxyReportValue = Proxy :: Proxy (ReportValue' params)

        -- Rener value
        renderCell :: forall v. ReportValue' params v => v -> ReaderHtml params ()
        renderCell v = td_ $ reportValueHtml v

        -- Column data to render controls
        colData :: NP ColumnData (Columns a)
        colData = SOP.hzipWith (ColumnData . unK) names columns'
          where
            columns' :: NP [] (Columns a)
            columns' = distributeNPList columns

        -- column names with types
        colNames :: [(Text, ColumnType)]
        colNames
            = SOP.hcollapse
            $ SOP.hcmap (Proxy :: Proxy ReportValue) f
            $ names
          where
            f :: forall x. ReportValue x => K Text x -> K (Text, ColumnType) x
            f (K name) = K (name, reportValueType (Proxy :: Proxy x))

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

-- | Class of types which are convertible to tabular data
--
-- Default definitions works for record like types using "Generics.SOP" machinery.
--
-- Another alternative for /scalar/ types is to wrap them:
--
-- @
-- instance ToColumns FUM.Login where
--     type Columns FUM.Login = '[FUM.Login]
--     columnNames _ = K "FUM" :* Nil
--     toColumns u   = [I u :* Nil]
-- @
--
-- However, most of these instance should be already defined in this module:
-- "Futurice.Report.Columns".
--
class ToColumns a where
    type Columns a :: [*]
    type Columns a =  DefaultColumns a

    columnNames :: Proxy a -> NP (K Text) (Columns a)
    default columnNames
        :: (SOP.IsProductType a xs, SOP.HasDatatypeInfo a, SListI xs, xs ~ Columns a)
        => Proxy a -> NP (K Text) (Columns a)
    columnNames = sopRecordFieldNames

    toColumns :: a -> [NP I (Columns a)]
    default toColumns
        :: ( code ~ Columns a
           , SOP.IsProductType a code
           , SOP.Generic a
           )
        => a -> [NP I (Columns a)]
    toColumns x = [SOP.unZ $ SOP.unSOP $ SOP.from x]

type DefaultColumns a = UnSingleton (SOP.Code a)

-------------------------------------------------------------------------------
-- Scalars
-------------------------------------------------------------------------------

instance ToColumns () where
    type Columns () = '[]

    columnNames _ = Nil
    toColumns _   = [Nil]

instance ToColumns FUM.Login where
    type Columns FUM.Login = '[FUM.Login]
    columnNames _ = K "fum" :* Nil
    toColumns u   = [I u :* Nil]

-- | TODO: differentiate differet names ('columnNames')
instance ToColumns (GH.Name a) where
    type Columns (GH.Name a) = '[GH.Name a]
    columnNames _ = K "gh" :* Nil
    toColumns n   = [I n :* Nil]

-------------------------------------------------------------------------------
-- Containers
-------------------------------------------------------------------------------

instance (ToColumns a, SListI (Columns a)) => ToColumns (Maybe a) where
    type Columns (Maybe a) = TMap Maybe (Columns a)

    columnNames _
        = recode (Proxy :: Proxy (Columns a))
        $ columnNames (Proxy :: Proxy a)
      where
        -- TODO: move to Futurice.List, implement using unsafeCoerce
        recode :: forall v xs. Proxy xs -> NP (K v) xs -> NP (K v) (TMap Maybe xs)
        recode _ Nil       = Nil
        recode _ (K x :* xs) = K x :* recode (p' xs) xs
          where
            p' :: forall ys. NP (K v) ys -> Proxy ys
            p' _ = Proxy

    toColumns :: Maybe a -> [NP I (TMap Maybe (Columns a))]
    toColumns Nothing =
        [ npCompToTMap (SOP.hpure inothing :: NP (I :.: Maybe) (Columns a)) ]
      where
        inothing :: forall b. (I :.: Maybe) b
        inothing = Comp (I Nothing)
    toColumns (Just a) = f <$> toColumns a
      where
        f :: NP I (Columns a) -> NP I (TMap Maybe (Columns a))
        f = npCompToTMap . SOP.hmap (Comp . fmap Just)

instance ToColumns a => ToColumns [a] where
    type Columns [a] = Columns a
    columnNames _ = columnNames (Proxy :: Proxy a)
    toColumns = concatMap toColumns

instance (ToColumns a, ToColumns b) => ToColumns (a, b) where
    type Columns (a, b) = Append (Columns a) (Columns b)
    columnNames _ = append
        (columnNames (Proxy :: Proxy a))
        (columnNames (Proxy :: Proxy b))
    toColumns (a, b) = append <$> toColumns a <*> toColumns b

instance (ToColumns a, ToColumns b) => ToColumns (S.Pair a b) where
    type Columns (S.Pair a b) = Append (Columns a) (Columns b)
    columnNames _ = append
        (columnNames (Proxy :: Proxy a))
        (columnNames (Proxy :: Proxy b))
    toColumns (a S.:!: b) = append <$> toColumns a <*> toColumns b

instance
    ( ToColumns a, ToColumns b
    , SListI (Columns a)
    , SListI (Columns b)
    ) => ToColumns (These a b)
  where
    type Columns (These a b) = Columns (Maybe a, Maybe b)
    columnNames _ = append
        (columnNames (Proxy :: Proxy (Maybe a)))
        (columnNames (Proxy :: Proxy (Maybe b)))
    toColumns (These a b) = toColumns (Just a, Just b)
    toColumns (This a)    = toColumns (Just a, Nothing :: Maybe b)
    toColumns (That b)    = toColumns (Nothing :: Maybe a, Just b)

instance ToColumns a => ToColumns (Vector a) where
    type Columns (Vector a) = Columns a
    columnNames _ = columnNames (Proxy :: Proxy a)
    toColumns = foldMap toColumns

instance (ToColumns k, ToColumns v) => ToColumns (Map k v) where
    type Columns (Map k v) = Append (Columns k) (Columns v)
    columnNames _ = append
        (columnNames (Proxy :: Proxy k))
        (columnNames (Proxy :: Proxy v))
    toColumns = ifoldMap f
      where
        f k v = append <$> toColumns k <*> toColumns v

instance (ToColumns k, ToColumns v) => ToColumns (HashMap k v) where
    type Columns (HashMap k v) = Append (Columns k) (Columns v)
    columnNames _ = append
        (columnNames (Proxy :: Proxy k))
        (columnNames (Proxy :: Proxy v))
    toColumns = ifoldMap f
      where
        f k v = append <$> toColumns k <*> toColumns v

-------------------------------------------------------------------------------
-- ReportGenerated
-------------------------------------------------------------------------------

-- | Report generation time.
newtype ReportGenerated = ReportGenerated { getReportGenerated :: UTCTime }
    deriving (Typeable, Generic)

instance NFData ReportGenerated
instance ToJSON ReportGenerated
instance FromJSON ReportGenerated
instance ToSchema ReportGenerated

instance ToHtml ReportGenerated where
    toHtml r = do
        "Generated at "
        toHtml . formatHumanHelsinkiTime . getReportGenerated $ r
        hr_ []
        i_ "Note: PlanMill data is updated at night around 03:30 – 05:00 (Finnish time)."
    toHtmlRaw = toHtml

-------------------------------------------------------------------------------
-- Cell value, i.e. primitive types shown in the report
-------------------------------------------------------------------------------

-- | Produce a tabular encoding of the report.
reportToTabularEncoding
    :: forall name params a.
       ( ToColumns a, All ToJSON (Columns a), All ReportValue (Columns a)
       , KnownSymbol name
       , ToJSON params
       )
    => Report name params a
    -> LazyByteString
reportToTabularEncoding (Report p d) = encodingToLazyByteString $ pairs $
    "name"   .= symbolVal (Proxy :: Proxy name) <>
    "params" .= p <>
    pair "columns" (list toEncoding $ SOP.hcollapse $ columnNames (Proxy :: Proxy a)) <>
    pair "types" (list toEncoding $ SOP.hcollapse $ generateColumnTypes proxyColumns) <>
    pair "data" (list toEncoding $ toColumns d)
  where
    proxyColumns = Proxy :: Proxy (Columns a)

-- | Generate types, so JS side knows how to render cells.
generateColumnTypes
    :: forall xs. (All ReportValue xs)
    => Proxy xs
    -> NP (K ColumnType) xs
generateColumnTypes _ = SOP.hcpure (Proxy :: Proxy ReportValue) f
  where
    f :: forall a. ReportValue a => K ColumnType a
    f = K $ reportValueType (Proxy :: Proxy a)

-- | Column type, for the JS frontend.
--
-- /TODO/ indicate if column can be nullable
data ColumnType
    = CTText     -- ^ text
    | CTNumber   -- ^ number
    | CTBool     -- ^ boolean
    | CTDay      -- ^ day: @2016-10-25@
    | CTDayDiff  -- ^ day difference: @5 days"
    | CTHourDiff -- ^ hour difference: @5 hours"
    | CTFumUser  -- ^ FUM user (e.g. abcd)
  deriving (Eq, Ord, Enum, Bounded, Generic, Typeable)

instance ToJSON ColumnType where
    toJSON CTText     = "text"
    toJSON CTNumber   = "number"
    toJSON CTBool     = "bool"
    toJSON CTDay      = "day"
    toJSON CTDayDiff  = "dayDiff"
    toJSON CTHourDiff = "hourDiff"
    toJSON CTFumUser  = "fumUser"

-- | Various aggregates
data Aggregate
    = AggFirst
    | AggCount
    | AggCountDistinct
    | AggSum
    -- | AggMin
    -- | AggMax
    | AggAvg
    | AggCollect
    | AggCollectDistinct
  deriving (Eq, Ord, Enum, Bounded, Generic, Typeable)

columnTypeAggregate :: ColumnType -> [Aggregate]
columnTypeAggregate CTText =
    [ AggFirst, AggCount, AggCountDistinct, AggCollect, AggCollectDistinct ]
columnTypeAggregate CTFumUser =
    [ AggFirst, AggCount, AggCountDistinct, AggCollect, AggCollectDistinct ]
columnTypeAggregate CTNumber =
    [ minBound .. maxBound ]
-- TODO: what aggregates makes sense for booleans? all, any?
columnTypeAggregate CTBool =
    [ AggFirst, AggCount, AggCollectDistinct ]
columnTypeAggregate CTDay =
    [ AggFirst, AggCount, AggCountDistinct, AggCollect, AggCollectDistinct ]
columnTypeAggregate CTDayDiff =
    [ minBound .. maxBound ]
columnTypeAggregate CTHourDiff =
    [ minBound .. maxBound ]

-- | Aggregaes' meta information
aggregateMeta :: Aggregate -> (Text, Text, Text)
aggregateMeta AggFirst           = ("∃", "first", "Pick first (random)")
aggregateMeta AggCount           = ("m", "count", "Count elements")
aggregateMeta AggCountDistinct   = ("n!", "countDistinct", "Count distinct elements")
aggregateMeta AggSum             = ("∑", "sum", "Sum elements")
aggregateMeta AggAvg             = ("μ", "avg", "Average of elements")
--aggregateMeta AggMax             = ("⊔", "max", "Maximum value")
--aggregateMeta AggMin             = ("⊓", "min", "Minimum value")
aggregateMeta AggCollect         = ("∀", "collect", "Collect all values")
aggregateMeta AggCollectDistinct = ("∀!", "collectDistinct", "Collect all distinct values")

-- |
--
class (ToJSON a, Ord a) => ReportValue a where
    type ReportValueC a :: * -> Constraint
    type ReportValueC a = Unit1

    -- | Render value to HTML
    --
    -- The value is shown only in static view, i.e. usually not for long.
    --
    reportValueHtml :: ReportValueC a env => a -> ReaderHtml env ()
    default reportValueHtml :: ReportValueC a env => ToHtml a => a -> ReaderHtml env ()
    reportValueHtml = hoist (\m _env -> runIdentity m) . toHtml

    -- | How JS frontend should handle / pretty-print the value
    --
    -- By default we treat everything as 'CTText'.
    --
    reportValueType :: Proxy a -> ColumnType
    reportValueType _ = CTText

-------------------------------------------------------------------------------
-- ReportValue instances
-------------------------------------------------------------------------------

data ColumnData a = ColumnData !Text ![a]

instance ReportValue Text

instance ReportValue Tribe
instance ReportValue Office

instance ReportValue Int where
    reportValueType _ = CTNumber
    reportValueHtml   = toHtml . show

instance ReportValue Integer where
    reportValueType _ = CTNumber
    reportValueHtml   = toHtml . show

instance ReportValue Word64 where
    reportValueType _ = CTNumber
    reportValueHtml   = toHtml . show

instance ReportValue Day where
    reportValueType _ = CTDay
    reportValueHtml = fromString . show

instance ReportValue UTCTime where
    reportValueHtml = fromString . show

instance ReportValue a => ReportValue (Maybe a) where
    type ReportValueC (Maybe a) = ReportValueC a
    reportValueType _ = reportValueType (Proxy :: Proxy a)
    reportValueHtml   = maybe (pure ()) reportValueHtml

instance ReportValue Bool where
    reportValueType _ = CTBool
    reportValueHtml   = bool "no" "yes"

instance ReportValue FUM.Login where
    reportValueType _ = CTFumUser
    type ReportValueC FUM.Login = Unit1
    reportValueHtml = toHtml

instance ReportValue a => ReportValue (FD.Identifier a res) where
    type ReportValueC (FD.Identifier a res) = ReportValueC a
    reportValueHtml   = reportValueHtml . FD.getIdentifier
    reportValueType _ = reportValueType (Proxy :: Proxy a)

instance ReportValue Personio.EmployeeId where
    reportValueType _ = CTText
    reportValueHtml = toHtml

instance ReportValue (PM.Identifier a) where
    reportValueType _            = CTNumber
    reportValueHtml (PM.Ident i) = toHtml (show i)

instance ReportValue (GH.Name a) where
    reportValueHtml = reportValueHtml . GH.untagName

instance
    (Ord a, Show a, NDTReportValue tu, AsScientific a)
    => ReportValue (NDT tu a)
  where
    reportValueType _ = ndtReportValueType (Proxy :: Proxy tu)
    reportValueHtml (NDT x) =
        toHtmlRaw $ show x <> nbsp <> sfx
      where
        sfx  = symbolVal (Proxy :: Proxy (TimeUnitSfx tu))
        nbsp = "&nbsp;" :: String

class IsTimeUnit tu => NDTReportValue tu where
    ndtReportValueType :: Proxy tu -> ColumnType
instance NDTReportValue 'Hours where ndtReportValueType _ = CTHourDiff
instance NDTReportValue 'Days  where ndtReportValueType _ = CTDayDiff

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | Move to "Futurice.SOP"
distributeNPList :: SListI xs => [NP I xs] -> NP [] xs
distributeNPList [] = SOP.hpure []
distributeNPList (x : xs) = SOP.hzipWith cons x (distributeNPList xs)
  where
    cons :: I a -> [a] -> [a]
    cons = (:) . unI
