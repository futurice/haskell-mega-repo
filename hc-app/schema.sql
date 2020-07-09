--CREATE SCHEMA hc;

CREATE TABLE hc.vacationreportlog (
  timestamp TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  receiver integer NOT NULL,
  sender text NOT NULL
);
