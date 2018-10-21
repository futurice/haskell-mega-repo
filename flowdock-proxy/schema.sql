CREATE SCHEMA "flowdock-proxy";

CREATE sequence "flowdock-proxy".flow_id_seq;

CREATE TABLE "flowdock-proxy".flows (
  flow_id integer not null default nextval('"flowdock-proxy".flow_id_seq'),
  flow_slug text not null,
  flow_name text not null,
  PRIMARY KEY (flow_id),
  UNIQUE (flow_slug)
);

INSERT INTO "flowdock-proxy".flows (flow_slug, flow_name) VALUES ('futurice', 'Futurice');

CREATE TABLE "flowdock-proxy".messages (
  flow_id integer NOT NULL,
  message_id integer NOT NULL,
  user_id integer NOT NULL,
  created timestamp with time zone NOT NULL,
  tags varchar(256)[] NOT NULL,
  content text NOT NULL,
  FOREIGN KEY (flow_id) REFERENCES "flowdock-proxy".flows (flow_id),
  PRIMARY KEY (flow_id, message_id)
);
