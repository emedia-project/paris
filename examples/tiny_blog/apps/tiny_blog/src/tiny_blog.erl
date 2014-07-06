-module(tiny_blog).

-export([start/0]).

start() ->
  {ok, _} = application:ensure_all_started(lager),
  {ok, _} = application:ensure_all_started(cowboy),
  ok = application:start(mimetypes),
  ok = application:start(tiny_blog),
  {ok, Conn} = texas:start(),
  ok = paris:set(db, Conn).
