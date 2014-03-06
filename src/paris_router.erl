-module(paris_router).

-export([
  init/3, 
  handle/2, 
  terminate/3
]).
-export([
  websocket_init/3,
  websocket_handle/3,
  websocket_info/3,
  websocket_terminate/3
]).

init({tcp, http}, Req, _Opts) ->
  case cowboy_req:header(<<"upgrade">>, Req) of
    {<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_websocket};
    {_, Req3} -> {ok, Req3, undefined_state}
  end.

% -------------------------------------
% REST
% -------------------------------------

handle(Req, State) ->
  {Method, _} = cowboy_req:method(Req),
  Action = list_to_atom(string:to_lower(binary_to_list(Method))),
  {Path, Module, Args} = get_module(Req),
  lager:info("~s ~s", [Method, Path]),
  {Code, Header, Body} = case code:ensure_loaded(Module) of
    {module, Module} ->
      case erlang:function_exported(Module, Action, 1+length(Args)) of
        true -> 
          erlang:apply(Module, Action, [Req] ++ Args);
        false -> 
          {404, [], []}
      end;
    _ ->
      static(Path)
  end,
  {ok, Reply} = if
    Code > 400 -> cowboy_req:reply(Code, Header, error_body(Code, Action, Path, Module), Req);
    true -> cowboy_req:reply(Code, Header, Body, Req)
  end,
  {ok, Reply, State}.

terminate(_Req, _State, _) ->
  ok.

% -------------------------------------
% Websocket
% -------------------------------------

websocket_init(TransportName, Req, Opts) ->
  {Path, Module, _Args} = get_module(Req),
  lager:debug("WS::init ~s", [Path]),
  erlang:apply(Module, init, [TransportName, Req, Opts]).

websocket_handle(Msg, Req, State) ->
  {Path, Module, _Args} = get_module(Req),
  lager:debug("WS::handle ~s", [Path]),
  erlang:apply(Module, handle, [Msg, Req, State]).

websocket_info(Info, Req, State) ->
  {Path, Module, _Args} = get_module(Req),
  lager:debug("WS::info ~s", [Path]),
  erlang:apply(Module, info, [Info, Req, State]).

websocket_terminate(Reason, Req, State) ->
  {Path, Module, _Args} = get_module(Req),
  lager:debug("WS::terminate ~s", [Path]),
  erlang:apply(Module, terminate, [Reason, Req, State]).

% -------------------------------------
% Private
% -------------------------------------

get_module(Req) ->
  {Path, _} = cowboy_req:path(Req),
  {Module, Args} = case string:tokens(binary_to_list(Path), "/") of
    [M|P] -> {list_to_atom(M), P};
    [] -> {index, []}
  end,
  {Path, Module, Args}.

error_body(Code, Action, Path, Module) ->
  ErrorTmpl = list_to_atom("error_" ++ integer_to_list(Code) ++ "_dtl"),
  case code:ensure_loaded(ErrorTmpl) of
    {module, ErrorTmpl} ->
      case ErrorTmpl:render([
            {action, atom_to_list(Action)},
            {path, binary_to_list(Path)},
            {controller, atom_to_list(Module)}
          ]) of
        {ok, IOList} -> IOList;
        _ -> []
      end;
    _ -> []
  end.

static(Path) ->
  Args = string:tokens(binary_to_list(Path), "/"),
  case lists:any(fun(E) -> E =:= ".." end, Args) of
    true -> {404, [], []};
    false ->
      case paris:priv_dir() of
        error -> {500, [], []};
        PrivDir -> 
          Static = filename:join([PrivDir, "static"] ++ Args),
          case file:read_file(Static) of
            {error, _} -> {404, [], []};
            {ok, Data} -> {200, [{<<"Content-Type">>, paris_utils:mime(Static)}], Data}
          end
      end
  end.

