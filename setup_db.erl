#!/usr/bin/env escript
%%! -pa deps/epgsql/ebin -Wall

-module(setup_db).

usage() ->
    io:format("usage: create - creates the DB tables\n"),
    io:format("       drop   - drop the DB tables\n"),
    halt(1).

main(["create"]) ->
    {ok, C} = pgsql:connect("localhost", "tim", [{database, "erlio"}]),
    {ok, _, _} = pgsql:squery(C, "create table links (id integer NOT NULL, link character varying(255), hits integer NOT NULL)"),
    {ok, _, _} = pgsql:squery(C, "CREATE SEQUENCE links_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
"),
    {ok, _, _} = pgsql:squery(C, "ALTER SEQUENCE links_id_seq OWNED BY links.id;"),
    ok = pgsql:close(C);
main(["drop"]) ->
    {ok, C} = pgsql:connect("localhost", "tim", [{database, "erlio"}]),
    {ok, _, _} = pgsql:squery(C, "drop table links"),
    ok = pgsql:close(C);
main(_) ->
    usage().
