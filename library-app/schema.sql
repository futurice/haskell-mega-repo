CREATE SCHEMA library;

CREATE TABLE library.loan_status (
    loan_id serial primary key,
    currently_loaned boolean NOT NULL,
    date_loaned date NOT NULL,
    personio_id integer NOT NULL
);

CREATE TABLE library.library (
    library_id serial primary key,
    name text NOT NULL
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

CREATE TABLE library.book (
    book_id serial primary key,
    bookinfo_id serial references library.bookinformation(bookinfo_id),
    library_id serial references library.library(library_id),
    loan_id serial references library.loan_status(loan_id)
);

CREATE TABLE library.other (
    object_id serial primary key,
    otherinfo_id serial references library.otherinformation(otherinfo_id),
    library_id serial references library.library(library_id),
    loan_id serial references library.loan_status(loan_id)
);
