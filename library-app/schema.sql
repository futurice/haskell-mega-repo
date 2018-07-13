CREATE SCHEMA library;

CREATE TABLE library.library (
    library_id serial primary key,
    name text NOT NULL
);

CREATE TABLE library.item (
    item_id serial primary key,
    item_type text NOT NULL,
    library_id integer references library.library(library_id)
);

CREATE TABLE library.old_loan (
    oldloan_id serial primary key,
    date_loaned date NOT NULL,
    date_returned date NOT NULL,
    personio_id integer NOT NULL,
    item_id integer references library.item(item_id)
);

CREATE TABLE library.loan (
    loan_id serial primary key,
    date_loaned date NOT NULL,
    personio_id integer NOT NULL,
    item_id serial references library.item(item_id)
);

CREATE TABLE library.bookinformation (
    bookinfo_id serial primary key,
    title text NOT NULL,
    isbn text NOT NULL,
    author text NOT NULL,
    publisher text NOT NULL,
    publishedYear smallint NOT NULL,
    cover text,
    amazon_link text
);

CREATE TABLE library.otherinformation (
    otherinfo_id serial primary key,
    name text NOT NULL
);

CREATE TABLE library.item_to_bookinformation (
    item_id serial references library.item(item_id),
    bookinfo_id serial references library.bookinformation(bookinfo_id),
    UNIQUE(item_id, bookinfo_id)
);

CREATE TABLE library.item_to_otherinformation (
    item_id serial references library.item(item_id),
    otherinfo_id serial references library.otherinformation(otherinfo_id),
    UNIQUE(item_id, otherinfo_id)
);
