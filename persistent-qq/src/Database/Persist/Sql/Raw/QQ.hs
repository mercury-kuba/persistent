{-|
@since 2.9.0

Module: module Database.Persist.Sql.Raw.QQ
Description: QuasiQuoters for performing raw sql queries

This module exports convenient QuasiQuoters to perform raw SQL queries.
All QuasiQuoters follow the same pattern and are analogous to the similar named
functions exported from 'Database.Persist.Sql'. Neither the quoted
function's behaviour, nor it's return value is altered during the translation
and all documentation provided with it holds.

The QuasiQuoters in this module perform a simple substitution on the query text,
that allows value substitutions, table name substitutions as well as column name
substitutions.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Database.Persist.Sql.Raw.QQ (
      -- * Sql QuasiQuoters
      queryQQ
    , queryResQQ
    , sqlQQ
    , executeQQ
    , executeCountQQ
    , ToRow(..)
    ) where

import Prelude
import Control.Arrow (first, second)
import Control.Monad.Reader (ask)
import           Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import Data.List (replicate, intercalate)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Monoid (mempty, (<>))
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse

import Database.Persist.Class (toPersistValue)
import Database.Persist
import Database.Persist.Sql

class ToRow a where
    toRow :: a -> NonEmpty PersistValue

instance PersistField a => ToRow (Single a) where
    toRow (Single a) = toPersistValue a :| []

instance (PersistField a, PersistField b) => ToRow (a, b) where
    toRow (a, b) = toPersistValue a <| toRow (Single b)

instance (PersistField a, PersistField b, PersistField c) => ToRow (a, b, c) where
    toRow (a, b, c) = toPersistValue a <| toRow (b, c)

instance (PersistField a, PersistField b, PersistField c, PersistField d) => ToRow (a, b, c, d) where
    toRow (a, b, c, d) = toPersistValue a <| toRow (b, c, d)

instance (PersistField a, PersistField b, PersistField c, PersistField d, PersistField e) => ToRow (a, b, c, d, e) where
    toRow (a, b, c, d, e) = toPersistValue a <| toRow (b, c, d, e)

data Token
  = Literal String
  | Value String
  | Values String
  | Rows String
  | TableName String
  | ColumnName String
  deriving Show

parseHaskell :: (String -> Token) -> String -> String -> [Token]
parseHaskell cons = go
  where
    go a []          = [Literal (reverse a)]
    go a ('\\':x:xs) = go (x:a) xs
    go a ['\\']      = go ('\\':a) []
    go a ('}':xs)    = cons (reverse a) : parseStr [] xs
    go a (x:xs)      = go (x:a) xs

parseStr :: String -> String -> [Token]
parseStr a []           = [Literal (reverse a)]
parseStr a ('\\':x:xs)  = parseStr (x:a) xs
parseStr a ['\\']       = parseStr ('\\':a) []
parseStr a ('#':'{':xs) = Literal (reverse a) : parseHaskell Value      [] xs
parseStr a ('%':'{':xs) = Literal (reverse a) : parseHaskell Values     [] xs
parseStr a ('*':'{':xs) = Literal (reverse a) : parseHaskell Rows       [] xs
parseStr a ('^':'{':xs) = Literal (reverse a) : parseHaskell TableName  [] xs
parseStr a ('@':'{':xs) = Literal (reverse a) : parseHaskell ColumnName [] xs
parseStr a (x:xs)       = parseStr (x:a) xs

interpolateValues :: PersistField a => NonEmpty a -> (String, [[PersistValue]]) -> (String, [[PersistValue]])
interpolateValues xs =
    first (mkPlaceholders values <>) .
    second (NonEmpty.toList values :)
  where
    values = NonEmpty.map toPersistValue xs

interpolateRows :: ToRow a => NonEmpty a -> (String, [[PersistValue]]) -> (String, [[PersistValue]])
interpolateRows xs (sql, vals) =
    (placeholders <> sql, values : vals)
  where
    rows :: NonEmpty (NonEmpty PersistValue)
    rows = NonEmpty.map toRow xs

    n = NonEmpty.length rows
    placeholders = n `timesCommaSeparated` mkPlaceholders (NonEmpty.head rows)
    values = List.concatMap NonEmpty.toList $ NonEmpty.toList rows

mkPlaceholders :: NonEmpty a -> String
mkPlaceholders values = "(" <> n `timesCommaSeparated` "?" <> ")"
  where
    n = NonEmpty.length values

timesCommaSeparated :: Int -> String -> String
timesCommaSeparated n = intercalate "," . replicate n

makeExpr :: TH.ExpQ -> [Token] -> TH.ExpQ
makeExpr fun toks = do
    [| do
        (sql, vals) <- $(go toks)
        $(fun) (Text.pack sql) (concat vals) |]
  where
    go :: [Token] -> TH.ExpQ
    go [] =
        [| return (mempty :: String, []) |]
    go (Literal a:xs) =
        [| first (a <>) <$> $(go xs) |]
    go (Value a:xs) = do
        [| (\(str, vals) -> ("?" <> str, [toPersistValue $(reifyExp a)] : vals)) <$> ($(go xs)) |]
    go (Values a:xs) =
        [| interpolateValues $(reifyExp a) <$> $(go xs) |]
    go (Rows a:xs) =
        [| interpolateRows $(reifyExp a) <$> $(go xs) |]
    go (ColumnName a:xs) =
        [| getFieldName $(reifyExp a) >>= \field ->
            first (Text.unpack field <>) <$> $(go xs) |]
    go (TableName a:xs) = do
        [| getTableName (error "record" :: $(TH.conT (TH.mkName a))) >>= \table ->
            first (Text.unpack table <>) <$> $(go xs) |]

reifyExp :: String -> TH.Q TH.Exp
reifyExp s =
    case parseExp s of
        Left e -> TH.reportError e >> [| mempty |]
        Right v -> return v

makeQQ :: TH.Q TH.Exp -> QuasiQuoter
makeQQ x = QuasiQuoter
    (makeExpr x . parseStr [])
    (error "Cannot use qc as a pattern")
    (error "Cannot use qc as a type")
    (error "Cannot use qc as a dec")

-- | QuasiQuoter for performing raw sql queries, analoguous to
-- 'Database.Persist.Sql.rawSql'
--
-- This and the following are convenient QuasiQuoters to perform raw SQL
-- queries.  They each follow the same pattern and are analogous to
-- the similarly named @raw@ functions.  Neither the quoted function's
-- behaviour, nor it's return value is altered during the translation and
-- all documentation provided with it holds.
--
-- These QuasiQuoters perform a simple substitution on the query text, that
-- allows value substitutions, table name substitutions as well as column name
-- substitutions.
--
-- Here is a small example:
--
-- Given the following simple model:
--
-- @
-- Category
--   rgt Int default=0
--   lft Int default=0
--   nam Text
-- @
--
-- We can now execute this raw query:
--
-- @
-- let lft = 10 :: `Int`
--     rgt = 20 :: `Int`
--     width = rgt `-` lft
--     nams = "first" `:|` ["second", "third"]
--  in [sqlQQ|
--       DELETE FROM ^{Category} WHERE \@{CategoryLft} BETWEEN \#{lft} AND \#{rgt};
--       UPDATE category SET \@{CategoryRgt} = \@{CategoryRgt} - \#{width} WHERE \@{CategoryRgt} > \#{rgt};
--       UPDATE category SET \@{CategoryLft} = \@{CategoryLft} - \#{width} WHERE \@{CategoryLft} > \#{rgt};
--       SELECT ?? FROM ^{Category} WHERE ^{Category}.\@{CategoryNam} IN %{nams};
--       INSERT INTO ^{Category}(\@{CategoryNam}) VALUES *{`Single` `<$>` nams};
--     |]
-- @
--
-- - @^{TableName}@ looks up the table's name and escapes it
-- - @\@{ColumnName}@ looks up the column's name and properly escapes it
-- - @#{value}@ inserts the value via the usual parameter substitution mechanism
-- - @%{values}@ inserts comma separated values (of a 'Data.List.NonEmpty.NonEmpty' list) (since 2.9.1)
-- - @*{rows}@ inserts a 'Data.List.NonEmpty.NonEmpty' list of tuples for use with a multirow @INSERT@ statement (since 2.9.2)
--
-- @since 2.9.0
sqlQQ :: QuasiQuoter
sqlQQ = makeQQ [| rawSql |]

-- | Analoguous to 'Database.Persist.Sql.rawExecute'
--
-- @since 2.9.0
executeQQ :: QuasiQuoter
executeQQ = makeQQ [| rawExecute |]

-- | Analoguous to 'Database.Persist.Sql.rawExecuteCount'
--
-- @since 2.9.0
executeCountQQ :: QuasiQuoter
executeCountQQ = makeQQ [| rawExecuteCount |]

-- | Analoguous to 'Database.Persist.Sql.rawQuery'
--
-- @since 2.9.0
queryQQ :: QuasiQuoter
queryQQ = makeQQ [| rawQuery |]

-- | Analoguous to 'Database.Persist.Sql.rawQueryRes'
--
-- @since 2.9.0
queryResQQ :: QuasiQuoter
queryResQQ = makeQQ [| rawQueryRes |]
