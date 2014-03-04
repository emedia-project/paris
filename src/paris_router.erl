-module(paris_router).

-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, _Opts) ->
  {ok, Req, undefined_state}.

handle(Req, State) ->
  {Method, _} = cowboy_req:method(Req),
  Action = list_to_atom(string:to_lower(binary_to_list(Method))),
  {Path, _} = cowboy_req:path(Req),
  {Module, Args} = case string:tokens(binary_to_list(Path), "/") of
    [M|P] -> {list_to_atom(M), P};
    [] -> {index, []}
  end,
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
