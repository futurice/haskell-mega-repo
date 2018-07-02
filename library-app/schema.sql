CREATE SCHEMA library;

CREATE SEQUENCE library.loan_seq;
CREATE SEQUENCE library.information_seq;

CREATE TABLE library.bookinformation (
    bookinfo_id integer primary key default nextval('library.information_seq'),
    title text NOT NULL,
    isbn text NOT NULL,
    author text NOT NULL,
    publisher text NOT NULL,
    publishedYear smallint NOT NULL,
    cover text,
    amazon_link text
);

CREATE TABLE library.boardgameinformation (
    boardgameinfo_id integer primary key default nextval('library.information_seq'),
    name text NOT NULL,
    publisher text,
    publishedYear smallint,
    designer text,
    artist text
);

CREATE TABLE library.item (
    item_id serial primary key,
    item_type text NOT NULL,
    library text NOT NULL,
    bookinfo_id integer references library.bookinformation(bookinfo_id),
    boardgameinfo_id integer references library.boardgameinformation(boardgameinfo_id),
    CONSTRAINT has_only_one_information_id CHECK ((bookinfo_id IS NULL)::integer + (boardgameinfo_id IS NULL)::integer = 1)
);

CREATE TABLE library.loan (
    loan_id integer primary key default nextval('library.loan_seq'),
    date_loaned date NOT NULL,
    personio_id integer NOT NULL,
    item_id integer NOT NULL references library.item(item_id)
);

CREATE TABLE library.old_loan (
    oldloan_id integer primary key default nextval('library.loan_seq'),
    date_loaned date NOT NULL,
    date_returned date NOT NULL,
    personio_id integer NOT NULL,
    item_id integer NOT NULL references library.item(item_id)
);
