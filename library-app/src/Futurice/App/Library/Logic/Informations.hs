{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Library.Logic.Informations (
    fetchInformationsWithCriteria,
    ) where

import Data.List                          (intersperse)
import Database.PostgreSQL.Simple.ToField (ToField (..), Action)
import Futurice.Postgres
import Futurice.Prelude
import Prelude ()

import Futurice.App.Library.Types

fetchInformationsWithCriteria
    :: forall m ctx ty. (Monad m, MonadLog m, MonadBaseControl IO m, MonadCatch m, HasPostgresPool ctx, SItemTypeI ty)
    => ctx
    -> SortCriteria ty
    -> Maybe (ItemInfo ty)
    -> SortDirection
    -> Int
    -> Maybe Text
    -> LibraryOrAll
    -> Bool
    -> m [ItemInfo ty]
fetchInformationsWithCriteria ctx crit mii direction limit search library onlyAvailable =
    fetchInfo (startInfo crit <$> mii)
  where
    whereWrapper = fold . intersperse " AND "

    fetchInfo :: (ToField c, ToField d) => Maybe (c, d) -> m [ItemInfo ty]
    fetchInfo info = case (info, search, library, onlyAvailable) of
        (Just (infoid, ii), Nothing, AllLibraries, False) ->
            safePoolQuery ctx
            (selectStatement crit <> " WHERE " <> startPos crit direction <> sortCriteria crit direction <> " LIMIT ?")
            (ii, infoid, limit)
        (Just (infoid, ii), Just s, AllLibraries, False) ->
            safePoolQuery ctx
            (selectStatement crit <> " WHERE " <> startPos crit direction <> " AND " <> searchSqlString crit <> sortCriteria crit direction <> " LIMIT ?")
            (ii, infoid, s, limit)
        (Nothing, Nothing, AllLibraries, False) ->
            safePoolQuery ctx
            (selectStatement crit <> sortCriteria crit direction <> " LIMIT ?")
            (Only limit)
        (Nothing, Just s, AllLibraries, False) ->
            safePoolQuery ctx
            (selectStatement crit <> " WHERE " <> searchSqlString crit <> sortCriteria crit direction <> " LIMIT ?")
            (s, limit)
        (Just (infoid, ii), Nothing, JustLibrary lib, False) ->
            safePoolQuery ctx
            (selectStatement crit <> " WHERE " <> startPos crit direction <> " AND " <> librarySqlString crit <> sortCriteria crit direction <> " LIMIT ?")
            (ii, infoid, lib, limit)
        (Just (infoid, ii), Just s, JustLibrary lib, False) ->
            safePoolQuery ctx
            (selectStatement crit <> " WHERE " <> startPos crit direction <> " AND " <> searchSqlString crit <> " AND " <> librarySqlString crit <> sortCriteria crit direction <> " LIMIT ?")
            (ii, infoid, s, lib, limit)
        (Nothing, Nothing, JustLibrary lib, False) ->
            safePoolQuery ctx
            (selectStatement crit <> " WHERE " <> librarySqlString crit <> sortCriteria crit direction <> " LIMIT ?")
            (lib, limit)
        (Nothing, Just s, JustLibrary lib, False) ->
            safePoolQuery ctx
            (selectStatement crit <> " WHERE " <> searchSqlString crit <> " AND " <> librarySqlString crit <> sortCriteria crit direction <> " LIMIT ?")
            (s, lib, limit)
        (Just (infoid, ii), Nothing, AllLibraries, True) ->
            safePoolQuery ctx
            (selectStatement crit <> " WHERE " <> startPos crit direction <> " AND " <> availabilitySqlString crit Nothing <> sortCriteria crit direction <> " LIMIT ?")
            (ii, infoid, limit)
        (Just (infoid, ii), Just s, AllLibraries, True) ->
            safePoolQuery ctx
            (selectStatement crit <> " WHERE " <> whereWrapper [startPos crit direction, searchSqlString crit, availabilitySqlString crit Nothing] <> sortCriteria crit direction <> " LIMIT ?")
            (ii, infoid, s, limit)
        (Nothing, Nothing, AllLibraries, True) ->
            safePoolQuery ctx
            (selectStatement crit <> " WHERE " <> availabilitySqlString crit Nothing <> sortCriteria crit direction <> " LIMIT ?")
            (Only limit)
        (Nothing, Just s, AllLibraries, True) ->
            safePoolQuery ctx
            (selectStatement crit <> " WHERE " <> whereWrapper [searchSqlString crit, availabilitySqlString crit Nothing] <> sortCriteria crit direction <> " LIMIT ?")
            (s, limit)
        (Just (infoid, ii), Nothing, JustLibrary lib, True) ->
            safePoolQuery ctx
            (selectStatement crit <> " WHERE " <> whereWrapper [startPos crit direction,librarySqlString crit, availabilitySqlString crit (Just lib)] <> sortCriteria crit direction <> " LIMIT ?")
            (ii, infoid, lib, lib, limit)
        (Just (infoid, ii), Just s, JustLibrary lib, True) ->
            safePoolQuery ctx
            (selectStatement crit <> " WHERE " <> whereWrapper [startPos crit direction, searchSqlString crit, librarySqlString crit, availabilitySqlString crit (Just lib)] <> sortCriteria crit direction <> " LIMIT ?")
            (ii, infoid, s, lib, lib, limit)
        (Nothing, Nothing, JustLibrary lib, True) ->
            safePoolQuery ctx
            (selectStatement crit <> " WHERE " <> whereWrapper [librarySqlString crit, availabilitySqlString crit (Just lib)] <> sortCriteria crit direction <> " LIMIT ?")
            (lib, lib, limit)
        (Nothing, Just s, JustLibrary lib, True) ->
            safePoolQuery ctx
            (selectStatement crit <> " WHERE " <> whereWrapper [searchSqlString crit, librarySqlString crit, availabilitySqlString crit (Just lib)] <> sortCriteria crit direction <> " LIMIT ?")
            (s, lib, lib, limit)

-------------------------------------------------------------------------------
-- Pieces
-------------------------------------------------------------------------------

selectStatement :: SortCriteria ty -> Query
selectStatement (BookSort _) = "SELECT bookinfo_id, title, isbn, author, publisher, publishedYear, cover, amazon_link FROM library.bookinformation "
selectStatement (BoardGameSort _) = "SELECT boardgameinfo_id, name, publisher, publishedYear, designer, artist FROM library.boardgameinformation "

startDirection :: SortDirection -> Query
startDirection SortDesc = " < "
startDirection SortAsc  = " > "

sortDirection :: SortDirection -> Query
sortDirection SortDesc = "DESC"
sortDirection SortAsc  = "ASC"

startPos :: SortCriteria ty -> SortDirection -> Query
startPos (BookSort SortTitle) dir         = " (title, bookinfo_id)" <> startDirection dir <> "(?,?) "
startPos (BookSort SortAuthor) dir        = " (author, bookinfo_id)" <> startDirection dir <> "(?,?) "
startPos (BookSort SortISBN) dir          = " (isbn, bookinfo_id)" <> startDirection dir <> "(?,?) "
startPos (BookSort SortPublished) dir     = " (publishedYear, bookinfo_id)" <> startDirection dir <> "(?,?) "
startPos (BoardGameSort SortName) dir     = " (name, boardgameinfo_id)" <> startDirection dir <> "(?,?) "
startPos (BoardGameSort SortDesigner) dir = " (designer, boardgameinfo_id)" <> startDirection dir <> "(?,?) "

sortCriteria :: SortCriteria ty -> SortDirection -> Query
sortCriteria (BookSort SortTitle) dir         = " ORDER BY title " <> sortDirection dir <> ", bookinfo_id "
sortCriteria (BookSort SortAuthor) dir        = " ORDER BY author " <> sortDirection dir <> ", bookinfo_id "
sortCriteria (BookSort SortISBN) dir          = " ORDER BY isbn " <> sortDirection dir <> ", bookinfo_id "
sortCriteria (BookSort SortPublished) dir     = " ORDER BY publishedYear " <> sortDirection dir <> ", bookinfo_id "
sortCriteria (BoardGameSort SortName) dir     = " ORDER BY name " <> sortDirection dir <> ", boardgameinfo_id "
sortCriteria (BoardGameSort SortDesigner) dir = " ORDER BY designer " <> sortDirection dir <> ", boardgameinfo_id "

-- Temporary HACK
startInfo :: SortCriteria ty -> ItemInfo ty -> (Action, Action)
startInfo (BookSort crit) (ItemBook bookInfo) =
    ( toField $ bookInfo ^. bookInformationId
    , toField $ startBookInfo crit bookInfo
    )
startInfo (BoardGameSort crit) (ItemBoardGame boardGameInfo) =
    ( toField $ boardGameInfo ^. boardGameInformationId
    , toField $ startBoardGameInfo crit boardGameInfo
    )

startBookInfo :: BookSortCriteria -> BookInformation -> Text
startBookInfo SortTitle     info = info ^. bookTitle
startBookInfo SortAuthor    info = info ^. bookAuthor
startBookInfo SortISBN      info = info ^. bookISBN
startBookInfo SortPublished info = textShow $ info ^. bookPublished

startBoardGameInfo :: BoardGameSortCriteria -> BoardGameInformation -> Text
startBoardGameInfo SortName     info = info ^. boardGameName
startBoardGameInfo SortDesigner info = fromMaybe "" $ info ^. boardGameDesigner

searchSqlString :: SortCriteria ty -> Query
searchSqlString (BookSort _) = " bookinfo_id in (SELECT bookinfo_id FROM (SELECT bookinfo_id, title, to_tsvector(title) || to_tsvector(isbn) || to_tsvector(author) || to_tsvector(publisher) || to_tsvector(to_char(publishedYear, '9999')) as document FROM library.bookinformation) book_search WHERE book_search.document @@ plainto_tsquery(?)) "
searchSqlString (BoardGameSort _) = " boardgameinfo_id in (SELECT boardgameinfo_id FROM (SELECT boardgameinfo_id, name, to_tsvector(name) || to_tsvector(publisher) || to_tsvector(to_char(publishedYear, '9999')) || to_tsvector(designer) || to_tsvector(artist) as document FROM library.boardgameinformation) boardgame_search WHERE boardgame_search.document @@ plainto_tsquery(?)) "

librarySqlString :: SortCriteria ty -> Query
librarySqlString (BookSort _) = " bookinfo_id in (select bookinfo_id from library.item where library = ? and bookinfo_id IS NOT NULL) "
librarySqlString (BoardGameSort _) = " boardgameinfo_id in (select boardgameinfo_id from library.item where library = ? and boardgameinfo_id IS NOT NULL) "

availabilitySqlString :: SortCriteria ty -> Maybe Library -> Query
availabilitySqlString (BookSort _) Nothing = " EXISTS (select * from library.item where bookinfo_id = library.bookinformation.bookinfo_id and item_id not in (select item_id from library.loan)) "
availabilitySqlString (BookSort _) (Just _) = " EXISTS (select * from library.item where bookinfo_id = library.bookinformation.bookinfo_id and library = ? and item_id not in (select item_id from library.loan)) "
availabilitySqlString (BoardGameSort _) Nothing = " EXISTS (select * from library.item where boardgameinfo_id = library.boardgameinformation.boardgameinfo_id and item_id not in (select item_id from library.loan)) "
availabilitySqlString (BoardGameSort _) (Just _) = " EXISTS (select * from library.item where boardgameinfo_id = library.boardgameinformation.boardgameinfo_id and library = ? and item_id not in (select item_id from library.loan)) "
