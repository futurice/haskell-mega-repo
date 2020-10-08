CREATE SEQUENCE library.category_seq;

CREATE TABLE library.bookcategory (
    category_id integer primary key default nextval('library.category_seq'),
    category_name text NOT NULL
);

CREATE SEQUENCE library.language_seq;

CREATE TABLE library.booklanguage (
    language_id integer primary key default nextval('library.language_seq'),
    language_name text NOT NULL
);

INSERT INTO library.booklanguage (language_name) VALUES ('Finnish'), ('English'), ('German');

ALTER TABLE library.bookinformation ADD COLUMN category_id integer references library.bookcategory(category_id);
ALTER TABLE library.bookinformation ADD COLUMN language_id integer references library.booklanguage(language_id);

CREATE SEQUENCE library.status_seq;

CREATE TABLE library.itemstatus (
    status_id integer primary key default nextval('library.status_seq'),
    status_name text NOT NULL
);

INSERT INTO library.itemstatus (status_name) VALUES ('Missing'), ('Marked for disposal');

ALTER TABLE library.item ADD COLUMN status_id integer references library.itemstatus(status_id);
