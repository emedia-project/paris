%% @doc
%% Routing
%% @end
-module(paris_router).

-export([
         routes/1,
         path/1
        ]).
-export([
         init/2
        ]).
-export([
         websocket_handle/3,
         websocket_info/3
        ]).

% -------------------------------------
% Common
% -------------------------------------

%% @hidden
routes(App) ->
  case application:get_env(App, routes) of
    {ok, Routes} ->
      create_routes(all, Routes);
    {ok, Type, Routes} ->
      create_routes(Type, Routes);
    _ ->
      create_routes(all, [])
  end.

%% @hidden
path(Module) ->
  case application:get_env(paris:app(), routes) of
    {ok, Routes} ->
      case elists:keyfind(Module, 2, Routes, <<>>) of
        <<>> ->
          case erlang:function_exported(Module, get, 1) of
            true ->
              "/" ++ eutils:to_list(Module);
            false ->
              case erlang:function_exported(Module, all, 1) of
                true ->
                  "/" ++ eutils:to_list(Module);
                false ->
                  "/?error=module"
              end
          end;
        Route -> Route
      end;
    _ ->
      "/?error=route"
  end.

% -------------------------------------
% REST
% -------------------------------------

%% @hidden
init(Req, Opts) ->
  case cowboy_req:header(<<"upgrade">>, Req) of
    <<"websocket">> ->
      {ok, Req2, Opts2} = websocket_init({}, Req, Opts),
      {cowboy_websocket, Req2, Opts2};
    _ -> handle(Req, Opts)
  end.

%% @hidden
handle(Req, State) ->
  {ok, ParisReq} = paris_req:start_link(Req),
  Method = paris_request:method(ParisReq),
  Action = list_to_atom(string:to_lower(binary_to_list(Method))),
  {Path, Module, Args} = get_module(ParisReq, State),
  {IP, _} = paris_request:peer(ParisReq),
  lager:info("~s - [~s] ~s ~s - ~p, ~p",
             [enet:ip_to_str(IP),
              edate:to_iso8601(edate:today()),
              Method, Path, Module, Args]),
  {Code, Header, Body} = try
                           case code:ensure_loaded(Module) of
                             {module, Module} ->
                               case before_actions(Module, ParisReq) of
                                 continue ->
                                   case erlang:function_exported(Module, Action, 1+length(Args)) of
                                     true ->
                                       erlang:apply(Module, Action, [ParisReq] ++ Args);
                                     false ->
                                       case erlang:function_exported(Module, all, 1+length(Args)) of
                                         true ->
                                           erlang:apply(Module, all, [ParisReq] ++ Args);
                                         false ->
                                           {404, [], []}
                                       end
                                   end;
                                 R -> R
                               end;
                             _ ->
                               case paris_plugin:call(controller, Module, Action, [ParisReq] ++ Args) of
                                 undef -> static(Path);
                                 R -> R
                               end
                           end
                         catch
                           _:_ -> {500, [], []}
                         end,
  Req4 = case Code of
           stream ->
             {Size, SendFile} = Body,
             Req2 = cowboy_req:set_resp_body_fun(Size, SendFile, paris_req:req(ParisReq)),
             cowboy_req:reply(200, Header, Req2);
           N when is_number(N) ->
             if
               Code >= 400 -> cowboy_req:reply(Code, Header,
                                               error_body(Body, Code, Action, Path, Module),
                                               paris_req:req(ParisReq));
               true -> cowboy_req:reply(Code, Header, Body, paris_req:req(ParisReq))
             end
         end,
  paris_req:stop(ParisReq),
  {ok, Req4, State}.


% -------------------------------------
% Websocket
% -------------------------------------

%% @hidden
websocket_init(TransportName, Req, Opts) ->
  {Path, Module, _Args} = get_module(Req, Opts),
  lager:debug("WS::init ~s", [Path]),
  erlang:apply(Module, init, [TransportName, Req, Opts]).

%% @hidden
websocket_handle(Msg, Req, State) ->
  {Path, Module, _Args} = get_module(Req, State),
  lager:debug("WS::handle ~s", [Path]),
  erlang:apply(Module, handle, [Msg, Req, State]).

%% @hidden
websocket_info(Info, Req, State) ->
  {Path, Module, _Args} = get_module(Req, State),
  lager:debug("WS::info ~s", [Path]),
  erlang:apply(Module, info, [Info, Req, State]).

% -------------------------------------
% Private
% -------------------------------------

create_routes(Type, Routes) ->
  CustomRoutes = lists:map(
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
                   lists:reverse(lists:sort(Routes))),
  [{'_',
    case Type of
      all -> CustomRoutes ++ [{'_', ?MODULE, []}];
      _ -> CustomRoutes
    end
   }].

get_module(ParisReq, State) ->
  Req = paris_req:req(ParisReq),
  Path = cowboy_req:path(Req),
  {Module, Args} = case State of
                     [M] ->
                       Args1 = case cowboy_req:path_info(Req) of
                                 undefined ->
                                   [];
                                 PathInfo ->
                                   [binary_to_list(E) || E <- PathInfo]
                               end,
                       {M, Args1};
                     [] ->
                       case string:tokens(binary_to_list(Path), "/") of
                         [M|P] -> {list_to_atom(M), P};
                         [] -> {index, []}
                       end
                   end,
  {Path, Module, Args}.

error_body([], Code, Action, Path, Module) ->
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
  end;
error_body(Body, _, _, _, _) ->
  Body.

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
