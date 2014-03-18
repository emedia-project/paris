-module(paris_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, [AppName]) ->
  Port = case application:get_env(AppName, port) of
    {ok, P} -> P;
    _ -> 8080
  end,
  IP = case application:get_env(AppName, ip) of
    {ok, I} ->
      [A, B, C, D] = [list_to_integer(X) || X <- string:tokens(I, ".")],
      {A, B, C, D};
    _ -> {0, 0, 0, 0}
  end,
  MaxConn = case application:get_env(AppName, max_conn) of
    {ok, MC} -> MC;
    _ -> 100
  end,
  Routes      = paris_router:routes(AppName),
  Dispatch    = cowboy_router:compile(Routes),
  TransOpts   = [{port, Port}, {ip, IP}],
  ProtoOpts   = [{env, [{dispatch, Dispatch}]}],
  {ok, _}     = cowboy:start_http(http, MaxConn, TransOpts, ProtoOpts),
  lager:info("Run mode : ~s", [os:getenv("PARIS_RUN_MODE")]),
  Mode = list_to_atom("paris_" ++ os:getenv("PARIS_RUN_MODE")),
  Mode:start(),
  lager:info("~p server started on port ~p", [AppName, Port]),
  paris_sup:start_link([{app, AppName}, {port, Port}, {ip, IP}, {max_conn, MaxConn}]).

stop(_State) ->
  ok.
