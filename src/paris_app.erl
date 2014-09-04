-module(paris_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, [AppName]) ->
  Port = case application:get_env(AppName, port) of
    {ok, P} -> P;
    _ ->
      case application:get_env(AppName, port_from) of
        {ok, {Module, Function, Args}} ->
          erlang:apply(Module, Function, Args);
        _ ->
          8080
      end
  end,
  StrIP = case application:get_env(AppName, ip) of
    {ok, I} -> I;
    _ ->
      case application:get_env(AppName, ip_from) of
        {ok, {Module1, Function1, Args1}} ->
          erlang:apply(Module1, Function1, Args1);
        _ -> "0.0.0.0"
      end
  end,
  [A, B, C, D] = [list_to_integer(X) || X <- string:tokens(StrIP, ".")],
  IP = {A, B, C, D},
  MaxConn = case application:get_env(AppName, max_conn) of
    {ok, MC} -> MC;
    _ ->
      case application:get_env(AppName, max_conn_from) of
        {ok, {Module2, Function2, Args2}} ->
          erlang:apply(Module2, Function2, Args2);
        _ -> 100
      end
  end,
  {SSL, SSLTransOpts} = case application:get_env(AppName, ssl) of
    {ok, []} -> 
      {false, []};
    {ok, Files} -> 
      {true, lists:map(fun({Type, F}) ->
              {Type,
               filename:join([code:priv_dir(AppName), "ssl", F])}
          end, Files)}; 
    _ -> {false, []}
  end,
  Routes      = paris_router:routes(AppName),
  Dispatch    = cowboy_router:compile(Routes),
  TransOpts   = [{port, Port}, {ip, IP}] ++ SSLTransOpts,
  ProtoOpts   = [{env, [{dispatch, Dispatch}]}],
  if
    SSL =:= true ->
      {ok, _} = cowboy:start_https(http, MaxConn, TransOpts, ProtoOpts);
    true ->
      {ok, _} = cowboy:start_http(https, MaxConn, TransOpts, ProtoOpts)
  end,
  lager:info("Run mode : ~s", [os:getenv("PARIS_RUN_MODE")]),
  Mode = list_to_atom("paris_" ++ os:getenv("PARIS_RUN_MODE")),
  Mode:start(),
  lager:info("~p server started on port ~p (ssl: ~p)", [AppName, Port, SSL]),
  case paris_sup:start_link([{app, AppName}, {port, Port}, {ip, IP}, {max_conn, MaxConn}]) of
    {ok, PPID} ->
      _ = case application:get_env(AppName, applications) of
        {ok, Apps} when is_list(Apps) -> 
          lists:foreach(fun(App) ->
                ok = application:start(App)
            end, Apps);
        _ -> ok
      end,
      {ok, PPID};
    E -> E
  end.

stop(_State) ->
  ok.
