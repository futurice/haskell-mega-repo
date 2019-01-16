create schema schedule;
set schema 'schedule';

create sequence schedule.command_id_seq;

create table schedule.commands (
    command_id int not null default nextval('schedule.command_id_seq') primary key,
    username text not null, -- the initiator of command, used of audit
    created timestamp with time zone not null default current_timestamp,
    command text not null, -- command name "create-event-template" or "add-to-schedule"
    payload jsonb not null -- command data, the payload
)
