-module(paris_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, [AppName]) ->
  Port = case application:get_env(AppName, port) of
    {ok, P} -> P;
    _ -> 
      case application:get_env(AppName, port_from) of
        {Module, Function, Args} -> erlang:apply(Module, Function, Args);
        _ -> 8080
      end
  end,
  IP = case application:get_env(AppName, ip) of
    {ok, I} ->
      [A, B, C, D] = [list_to_integer(X) || X <- string:tokens(I, ".")],
      {A, B, C, D};
    _ -> 
      case application:get_env(AppName, ip_from) of
        {Module1, Function1, Args1} -> erlang:apply(Module1, Function1, Args1);
        _ -> {0, 0, 0, 0}
      end
  end,
  MaxConn = case application:get_env(AppName, max_conn) of
    {ok, MC} -> MC;
    _ -> 
      case application:get_env(AppName, max_conn_from) of
        {Module2, Function2, Args2} -> erlang:apply(Module2, Function2, Args2);
        _ -> 100
      end
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
