--INSERT INTO library.itemstatus (status_name) VALUES ('Missing'), ('Marked for disposal');

ALTER TABLE library.bookinformation ADD COLUMN category text;
ALTER TABLE library.bookinformation ADD COLUMN language text;
ALTER TABLE library.item ADD COLUMN status text;
