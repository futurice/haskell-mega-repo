CREATE SCHEMA library;

CREATE TABLE library.library_book (
    id bigint NOT NULL,
    information_id bigint NOT NULL,
    library_id bigint NOT NULL
);

CREATE TABLE library.library_bookinformation (
    id bigint NOT NULL,
    title character varying(255) NOT NULL,
    isbn character varying(255) NOT NULL,
    author character varying(255) NOT NULL,
    publisher character varying(255) NOT NULL,
    published bigint NOT NULL,
    cover character varying(100),
    amazon_link character varying(1024)
);

CREATE TABLE library.library_library (
    id bigint NOT NULL,
    name character varying(255) NOT NULL
);

CREATE TABLE library.library_loan (
    id bigint NOT NULL,
    date_loaned date NOT NULL,
    book_id bigint NOT NULL,
    person_id bigint NOT NULL,
    personio_id bigint
);
