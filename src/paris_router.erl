-module(paris_router).

-export([
  routes/1
]).
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

% -------------------------------------
% Common
% -------------------------------------

routes(App) ->
  CustomRoutes = case application:get_env(App, routes) of
    {ok, Routes} -> 
      lists:map(
        fun
          ({Path, {redirect, Uri, Params}}) ->
            {Path, paris_redirect, [Uri, Params]};
          ({Path, Module}) ->
            [T|_] = lists:reverse(Path),
            if 
              T =:= $/ -> {Path ++ "[...]", ?MODULE, [Module]};
              true -> {Path ++ "/[...]", ?MODULE, [Module]}
            end
        end,
        lists:reverse(lists:sort(Routes)));
    _ -> 
      []
  end,
  [{'_', 
    CustomRoutes ++ [{'_', ?MODULE, []}]
  }].

% -------------------------------------
% REST
% -------------------------------------

init(_, Req, Opts) ->
  case cowboy_req:header(<<"upgrade">>, Req) of
    {<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_websocket};
    {_, Req3} -> {ok, Req3, Opts}
  end.

handle(Req, State) ->
  {Method, _} = cowboy_req:method(Req),
  Action = list_to_atom(string:to_lower(binary_to_list(Method))),
  {Path, Module, Args} = get_module(Req, State),
  lager:info("~s ~s (~p :: ~p)", [Method, Path, Module, Args]),
  {Code, Header, Body} = try
    case code:ensure_loaded(Module) of
      {module, Module} ->
        case before_actions(Module, Req) of
          continue ->
            case erlang:function_exported(Module, Action, 1+length(Args)) of
              true -> 
                erlang:apply(Module, Action, [Req] ++ Args);
              false -> 
                case erlang:function_exported(Module, all, 1+length(Args)) of
                  true -> 
                    erlang:apply(Module, all, [Req] ++ Args);
                  false ->
                    {404, [], []}
                end
            end;
          R -> R
        end;
      _ ->
        case paris_plugin:call(controller, Module, Action, [Req] ++ Args) of
          undef -> static(Path);
          R -> R
        end
    end
  catch 
    _:_ -> {500, [], "error 5000"}
  end,
  {ok, Req4} = case Code of
    stream -> 
      {Size, SendFile} = Body,
      Req2 = cowboy_req:set_resp_body_fun(Size, SendFile, Req),
      cowboy_req:reply(200, Header, Req2);
    N when is_number(N) ->
      if
        Code > 400 -> cowboy_req:reply(Code, Header, error_body(Code, Action, Path, Module), Req);
        true -> cowboy_req:reply(Code, Header, Body, Req)
      end
  end,
  {ok, Req4, State}.


terminate(_Req, _State, _) ->
  ok.

% -------------------------------------
% Websocket
% -------------------------------------

websocket_init(TransportName, Req, Opts) ->
  {Path, Module, _Args} = get_module(Req, Opts),
  lager:debug("WS::init ~s", [Path]),
  erlang:apply(Module, init, [TransportName, Req, Opts]).

websocket_handle(Msg, Req, State) ->
  {Path, Module, _Args} = get_module(Req, State),
  lager:debug("WS::handle ~s", [Path]),
  erlang:apply(Module, handle, [Msg, Req, State]).

websocket_info(Info, Req, State) ->
  {Path, Module, _Args} = get_module(Req, State),
  lager:debug("WS::info ~s", [Path]),
  erlang:apply(Module, info, [Info, Req, State]).

websocket_terminate(Reason, Req, State) ->
  {Path, Module, _Args} = get_module(Req, State),
  lager:debug("WS::terminate ~s", [Path]),
  erlang:apply(Module, terminate, [Reason, Req, State]).

% -------------------------------------
% Private
% -------------------------------------

get_module(Req, State) ->
  {Path, _} = cowboy_req:path(Req),
  {Module, Args} = case State of
    [M] -> 
      Args1 = case cowboy_req:path_info(Req) of
        {PathInfo, _} when is_list(PathInfo) ->
          [binary_to_list(E) || E <- PathInfo];
        _ -> 
          []
      end,
      {M, Args1};
    [] ->
      case string:tokens(binary_to_list(Path), "/") of
        [M|P] -> {list_to_atom(M), P};
        [] -> {index, []}
      end
  end,
  {Path, Module, Args}.

error_body(Code, Action, Path, Module) ->
  ErrorTmpl = list_to_atom("error_" ++ integer_to_list(Code) ++ "_html"),
  Template = case code:ensure_loaded(ErrorTmpl) of
    {module, ErrorTmpl} -> ErrorTmpl;
    _ -> paris_error_view_dtl
  end,
  case Template:render([
        {code, Code},
        {action, atom_to_list(Action)},
        {path, binary_to_list(Path)},
        {controller, atom_to_list(Module)},
        {stacktrace, io_lib:format("~p", [erlang:get_stacktrace()])}
        ]) of
    {ok, IOList} -> IOList;
    _ -> []
  end.

static(Path) ->
  Args = string:tokens(binary_to_list(Path), "/"),
  case lists:any(fun(E) -> E =:= ".." end, Args) of
    true -> {404, [], []};
    false ->
      Static = paris:static(Args),
      case filelib:is_dir(Static) of
        true ->
          case filelib:is_file(filename:join(Static, "index.html")) of
            true ->
              {302, [{<<"Location">>, filename:join(["/"] ++ Args ++ ["index.html"])}], []};
            false ->
              {404, [], []}
          end;
        _ ->
          case file:read_file(Static) of
            {error, _} -> {404, [], []};
            {ok, Data} -> {200, [{<<"Content-Type">>, paris_utils:mime(Static)}], Data}
          end
      end
  end.

before_actions(Module, Req) ->
  case erlang:function_exported(Module, before, 1) of
    true -> case erlang:apply(Module, before, [Req]) of 
              continue -> continue;
              {_, _, _} = R -> R
            end;
    false -> continue
  end.
