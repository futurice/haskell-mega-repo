{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
-- | This small library helps with building complex queries:
--
-- >>> :{
-- demo $ do
--     tbl <- from_ "table1"
--     fields_ tbl [ "c1", "c2"]
--     orderby_ tbl "c2" ASC
--     where_ tbl "c3" $ \c3 -> param1_ (-100 :: Int)     [ "? < ",  c3 ]
--     where_ tbl "c3" $ \c3 -> param1_ (100 :: Int)      [ c3, " < ?" ]
--     where_ tbl "c4" $ \c4 -> param1_ ("foo" :: String) [ c4, " = ?" ]
-- :}
-- SELECT t.c1, t.c2
-- FROM sch.table1 t
-- WHERE (? < t.c3) AND (t.c3 < ?) AND (t.c4 = ?)
-- ORDER BY t.c2 ASC
-- ---
-- Plain "-100"
-- Plain "100"
-- Escape "foo"
module Futurice.Postgres.SqlBuilder (
    -- * Running queries
    poolQueryM,
    safePoolQueryM,
    -- * DSL
    -- ** Methods
    from_,
    fromSubselect_,
    fields_,
    orderby_,
    where_,
    -- ** Types
    QueryM,
    TableName,
    TableAbbr,
    ColumnName,
    SortDirection (..),
    -- * Utilities
    ToExpression,
    noParam_,
    param1_,
    params_,
    -- * Debug
    renderQuery,
    ) where

import Control.Monad          (ap)
import Data.Char              (isAlphaNum)
import Data.Semigroup.Generic (gmappend, gmempty)
import Futurice.Postgres
import Futurice.Prelude
import Prelude ()

import qualified Data.Set                           as Set
import qualified Database.PostgreSQL.Simple         as PQ
import qualified Database.PostgreSQL.Simple.ToField as PQ
import qualified Database.PostgreSQL.Simple.ToRow   as PQ
import qualified Text.PrettyPrint.Compact           as PP

-------------------------------------------------------------------------------
-- Running
-------------------------------------------------------------------------------

poolQueryM
    :: (PQ.FromRow r, HasPostgresPool ctx, MonadBaseControl IO m)
    => ctx -> String -> QueryM () -> m [r]
poolQueryM ctx schema q = poolQuery ctx (fromString query) row
  where
    (query, row) = renderQuery schema q

safePoolQueryM
    :: (PQ.FromRow r, HasPostgresPool ctx, MonadBaseControl IO m, MonadLog m, MonadCatch m)
    => ctx -> String -> QueryM () -> m [r]
safePoolQueryM ctx schema q = safePoolQuery ctx (fromString query) row
  where
    (query, row) = renderQuery schema q

-------------------------------------------------------------------------------
-- Newtypes
-------------------------------------------------------------------------------

newtype TableName = TN String deriving (Show, IsString)
newtype ColumnName = CN String deriving (Show, IsString)

-- | Table abbreviation.
--
-- Returned by 'from_'
newtype TableAbbr = TableAbbr String
  deriving (Eq, Ord, Show)

-------------------------------------------------------------------------------
-- Query
-------------------------------------------------------------------------------

data SortDirection = ASC | DESC
  deriving (Show)

data SelectQuery = SelectQuery
    { sqTables  :: [(Either SelectQuery TableName, TableAbbr)]
    , sqFields  :: [(TableAbbr, ColumnName)]
    , sqOrderBy :: [(TableAbbr, ColumnName, SortDirection)]
    , sqWhere   :: [ToExpr]
    }
  deriving (Show, Generic)

instance Semigroup SelectQuery where
    (<>) = gmappend

instance Monoid SelectQuery where
    mempty = gmempty
    mappend = (<>)

-------------------------------------------------------------------------------
-- Query Monad
-------------------------------------------------------------------------------

-- | Query building monad.
newtype QueryM a = QueryM { unQueryM :: St -> (SelectQuery, St, a) }
  deriving (Functor)

instance Applicative QueryM where
    pure = return
    (<*>) = ap

instance Monad QueryM where
    return x = QueryM $ \s -> (mempty, s, x)

    m >>= k = QueryM $ \st0 ->
        let (sq1, st1, x) = unQueryM m st0
            (sq2, st2, y) = unQueryM (k x) st1
        in (sq1 <> sq2, st2, y)

-- | State of 'QueryM'.
newtype St = St
    { stAbbrs :: Set TableAbbr
    }

emptyS :: St
emptyS = St
    { stAbbrs = mempty
    }

-------------------------------------------------------------------------------
-- DSL
-------------------------------------------------------------------------------

-- | @FROM table1@
--
-- >>> demo $ from_ "table1"
-- SELECT 1 FROM sch.table1 t
--
-- >>> demo $ from_ "table-with-dash" >>= \tbl -> fields_ tbl [ "c1", "c2" ]
-- SELECT t.c1, t.c2 FROM sch."table-with-dash" t
--
from_ :: TableName -> QueryM TableAbbr
from_ (TN tbl) = QueryM $ \(St tbls) ->
    let abbr = takeAbbr $ filter (\a -> Set.notMember a tbls) abbrs
    in (mempty { sqTables = [ (Right (TN tbl), abbr) ] } , St $ Set.insert abbr tbls, abbr)
  where
    abbrs = case tbl of
        []    -> [ TableAbbr $ "tmp" ++ show i | i <- [ 0 :: Int .. ] ]
        (c:_) -> TableAbbr [c] : [ TableAbbr $ c : show i | i <- [ 1 :: Int .. ] ]

    takeAbbr []         = TableAbbr "panic" -- should not happen
    takeAbbr (abbr : _) = abbr

-- | @FROM (SELECT ...)@
--
-- >>> let q = from_ "table1" >>= \tbl -> fields_ tbl [ "col1", "col2" ]
-- >>> demo $ fromSubselect_ q >>= \tbl -> fields_ tbl [ "col1" ]
-- SELECT sub0.col1 FROM (SELECT t.col1, t.col2 FROM sch.table1 t) sub0
--
fromSubselect_ :: QueryM () -> QueryM TableAbbr
fromSubselect_ subQ = QueryM $ \st0 ->
    let (q, St tbls, ()) = unQueryM subQ st0
        abbr = takeAbbr $ filter (\a -> Set.notMember a tbls) abbrs
    in (mempty { sqTables = [(Left q, abbr)] }, St $ Set.insert abbr tbls, abbr)
  where
    abbrs = [ TableAbbr $ "sub" ++ show i | i <- [ 0 :: Int .. ] ]

    takeAbbr []         = TableAbbr "panic" -- should not happen
    takeAbbr (abbr : _) = abbr

-- | @SELECT col1, col2@
--
-- >>> demo $ from_ "table1" >>= \tbl -> fields_ tbl [ "col1", "col2" ]
-- SELECT t.col1, t.col2 FROM sch.table1 t
--
fields_ :: TableAbbr -> [ColumnName] -> QueryM ()
fields_ tbl cls = QueryM $ \st ->
    (mempty { sqFields = [ (tbl, cl) | cl <- cls ] }, st, ())

-- | @ORDER BY tbl.clmn ASC@
--
-- >>> demo $ from_ "table1" >>= \tbl -> fields_ tbl [ "col1", "col2" ] >> orderby_ tbl "col1" ASC
-- SELECT t.col1, t.col2 FROM sch.table1 t ORDER BY t.col1 ASC
--
orderby_ :: TableAbbr -> ColumnName -> SortDirection -> QueryM ()
orderby_ tbl cl sd = QueryM $ \st ->
    (mempty { sqOrderBy = [ (tbl, cl, sd) ] }, st, ())

-- | @WHERE tbl.clmn = ?@
--
-- Where clauses referencing single column.
--
-- >>> demo $ from_ "table1" >>= \tbl -> fields_ tbl [ "c1", "c2"] >> where_ tbl "c3" (\c3 -> noParam_ [ c3, " < 42" ])
-- SELECT t.c1, t.c2 FROM sch.table1 t WHERE (t.c3 < 42)
--
-- >>> demo $ from_ "table1" >>= \tbl -> fields_ tbl [ "c1", "c2"] >> where_ tbl "c3" (\c3 -> params_ (-100 :: Int, 100 :: Int) [ "? < ", c3, " AND ", c3, " < ?" ])
-- SELECT t.c1, t.c2 FROM sch.table1 t WHERE (? < t.c3 AND t.c3 < ?)
-- ---
-- Plain "-100"
-- Plain "100"
--
-- /TODO:/ add @whereMany_@ to make where clauses involving multiple columns. Use 'Each' from @lens@.
--
where_
    :: TableAbbr -> ColumnName
    -> (forall col expr. ToExpression col expr => col -> expr)
    -> QueryM ()
where_ tbl cl f = QueryM $ \st ->
    (mempty { sqWhere = [ f $ escapeTableColumn tbl cl ] }, st, ())

-- | Class for building expressions, e.g. in 'where_' clauses.
class IsString col => ToExpression col expr | expr -> col where
    noParam_ :: [col] -> expr
    param1_  :: PQ.ToField f => f  -> [col] -> expr
    params_  :: PQ.ToRow fs  => fs -> [col] -> expr

data ToExpr = ToExpr String [PQ.Action]
  deriving Show

-- | This "smart" constructor checks that length of actions equals
-- the '?' signs in a query part.
--
-- Otherwise 'error's early.
--
toExpr :: String -> [PQ.Action] -> ToExpr
toExpr = ToExpr

instance ToExpression [Char] ToExpr where
    noParam_ cols   = toExpr (mconcat cols) []
    param1_ f  cols = toExpr (mconcat cols) [PQ.toField f]
    params_ fs cols = toExpr (mconcat cols) (PQ.toRow fs)

-------------------------------------------------------------------------------
-- Escaping table column names
-------------------------------------------------------------------------------

escapeTableColumn :: TableAbbr -> ColumnName -> String
escapeTableColumn (TableAbbr tn) (CN cn) =
    escapeSymbol tn ++ "." ++ escapeSymbol cn

ppTableColumn :: TableAbbr -> ColumnName -> PP.Doc ()
ppTableColumn tbl cl = PP.text (escapeTableColumn tbl cl)

escapeSymbol :: String -> String
escapeSymbol str
    | all isAlphaNum str = str
    | otherwise          = "\"" ++ str ++ "\""

-------------------------------------------------------------------------------
-- Rendering
-------------------------------------------------------------------------------

-- | Render query into template and pieces.
renderQuery
    :: String                -- ^ schema
    -> QueryM ()             -- ^ query builder
    -> (String, [PQ.Action]) -- ^ query template and actions (from @postgresql-simple@)
renderQuery sch (QueryM f) = case f emptyS of
    (sq, _st, ()) -> (PP.render $ ppQuery sch sq, queryActions sq)

ppQuery :: String ->  SelectQuery -> PP.Doc ()
ppQuery schema sq = PP.sep $
    [ PP.hang 4 (PP.text "SELECT") $ ppFields $ sqFields sq
    ] ++ catMaybes
    [ if null (sqTables sq)
      then Nothing
      else Just $ PP.hang 4 (PP.text "FROM") $ ppTables $ sqTables sq
    , if null (sqWhere sq)
      then Nothing
      else Just $ PP.hang 4 (PP.text "WHERE") $ ppWhere $ sqWhere sq
    , if null (sqOrderBy sq)
      then Nothing
      else Just $ PP.hang 4 (PP.text "ORDER BY") $ ppOrderBy $ sqOrderBy sq
    ]
  where
    ppFields :: [(TableAbbr, ColumnName)] -> PP.Doc ()
    ppFields [] = PP.text "1"
    ppFields xs = PP.sep $ PP.punctuate PP.comma
        [ ppTableColumn tbl cl
        | (tbl, cl) <- xs
        ]

    ppTables :: [(Either SelectQuery TableName, TableAbbr)] -> PP.Doc ()
    ppTables xs = PP.sep $ PP.punctuate PP.comma
        [ ppTable tblOrQuery PP.<+> PP.text abbr
        | (tblOrQuery, TableAbbr abbr) <- xs
        ]
      where
        ppTable :: Either SelectQuery TableName -> PP.Doc ()
        ppTable (Right (TN tbl)) = PP.text (escapeSymbol schema ++ "." ++ escapeSymbol tbl)
        ppTable (Left sq')       = PP.parens (ppQuery schema sq')

    ppOrderBy :: [(TableAbbr, ColumnName, SortDirection)] -> PP.Doc ()
    ppOrderBy xs = PP.sep $ PP.punctuate PP.comma
        [ ppTableColumn tbl cl PP.<+> PP.text (show sd)
        | (tbl, cl, sd) <- xs
        ]

    ppWhere :: [ToExpr] -> PP.Doc ()
    ppWhere xs = PP.sep $ PP.punctuate (PP.text " AND")
        [ PP.parens $  PP.text str
        | ToExpr str _ <- xs
        ]

-- | Note: be careful to produce actions in the same order as in 'ppQuery'.
queryActions :: SelectQuery -> [PQ.Action]
queryActions sq = mconcat
    [ concatMap whereAction (sqWhere sq)
    ]
  where
    whereAction (ToExpr _ acts) = acts

-------------------------------------------------------------------------------
-- ...
-------------------------------------------------------------------------------

-- $setup
--
-- >>> :set -XOverloadedStrings -Wno-type-defaults
-- >>> let demo q = let (str, acts) = renderQuery "sch" (void q) in putStrLn str >> unless (null acts) (putStrLn "---") >> traverse_ print acts
