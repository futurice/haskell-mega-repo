CREATE SCHEMA library;

-- See library.item description
CREATE SEQUENCE library.information_seq;

-- Comments are pseudo-Haskell, to highlight the relations

-- Library contains books, which are records
--
--     data BookInfo = BookInfo { ... }
--
-- and have an "identity"
--
--     class    HasInfoId a        where infoId :: Lens a InfoId
--     instance HasInfoId BookInfo where infoId = ...
--
CREATE TABLE library.bookinformation (
    bookinfo_id integer primary key default nextval('library.information_seq'),
    title text NOT NULL,
    isbn text NOT NULL,
    author text NOT NULL,
    publisher text NOT NULL,
    publishedYear smallint NOT NULL,
    cover text,
    info_link text
);

-- Board games are another type of items we have in the library
--
--     data BoardGameInfo = BoardGameInfo { ... }
--     instance HasInfoId BoardGameInfo where infoId = ...
--
CREATE TABLE library.boardgameinformation (
    boardgameinfo_id integer primary key default nextval('library.information_seq'),
    name text NOT NULL,
    publisher text,
    publishedYear smallint,
    designer text,
    artist text
);

-- Items are concrete instances of the books or boardgames we have.
--
--     data Item = Item
--         { itemId       :: ItemId
--         , itemLibrary  :: Library
--         , itemInfo     :: ItemInfo
--         }
--
-- where ItemInfo both tells the type of the item (book, ...) and
-- points to that type. It's represented by the foreign keys in the db.
--
--     data ItemInfo
--         = ItemBook BookInfo
--         | ItemBoardGame BoardGameInfo
--
--     instance HasInfoId ItemInfo where infoId = ...
--     instance HasInfoId Item     where infoId = ...
--
-- We could have an info lookup function, if we have the complete World-view.
--
--     getItemInfo :: World -> InfoId -> Maybe ItemInfo
--
-- One analogy is to think about ItemInfo as a "class" (OO-wise),
-- and Item as an "object". For example we have only one
--
--     perlBook :: BookInfo
--     perlBook = BookInfo { title = "Advanced Perl Programming", ... }
--
-- but might have multiple items, say two in Helsinki and one in Tampere
-- (google for Advanced Perl Programming, it's available online).
--
--     perlBook0 = Item { itemInfo = BookInfo perlBook, itemLibrary = "Helsinki", itemId = 1001 }
--     perlBook1 = Item { itemInfo = BookInfo perlBook, itemLibrary = "Helsinki", itemId = 1002 }
--     perlBook2 = Item { itemInfo = BookInfo perlBook, itemLibrary = "Tampere",  itemId = 1003 }
--
CREATE TABLE library.item (
    item_id serial primary key,
    library text NOT NULL,
    bookinfo_id integer references library.bookinformation(bookinfo_id),
    boardgameinfo_id integer references library.boardgameinformation(boardgameinfo_id),
    CONSTRAINT has_only_one_information_id CHECK ((bookinfo_id IS NULL)::integer + (boardgameinfo_id IS NULL)::integer = 1)
);

-- Loans are divided into two groups, active loans (this table)
-- and returned loans (old_loans).
--
-- Active loan is an Item loaned by a person (personio_id).
--
--     data Loan = Loan
--         { loanId     :: LoanId
--         , loanItem   :: Item
--         , loanPerson :: PersonioId  -- or even "Maybe Personio.Employee"
--         , loanDate   :: Day
--         }
--
-- Note: the item_id has an UNIQUE constraint. An item can be loaned only once.
--
CREATE TABLE library.loan (
    loan_id serial primary key,
    date_loaned date NOT NULL,
    personio_id integer NOT NULL,
    item_id integer NOT NULL UNIQUE references library.item(item_id)
);

-- When loans are returned, the 'loan' rows are moved to this table.
-- Here we don't have an unique constraint anymore.
--
--     data RetunedLoan = ReturnedLoan
--         { returnedLoan :: Loan  -- original loan
--         , returnedData :: Day
--         }
--
CREATE TABLE library.old_loan (
    oldloan_id integer primary key,
    date_loaned date NOT NULL,
    date_returned date NOT NULL,
    personio_id integer NOT NULL,
    item_id integer NOT NULL references library.item(item_id)
);
