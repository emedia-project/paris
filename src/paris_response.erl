-module(paris_response).

-export([
  cookie/4,
  delete_cookie/2,
  render/2,
  redirect/1,
  redirect/2,
  http_error/1,
  http_error/2,
  http_error/3,

  ws_terminate/0,
  ws_ok/2,
  ws_ok/3,
  ws_hibernate/2,
  ws_hibernate/3,
  ws_shutdown/2,
  ws_text/3,
  ws_json/3,
  ws_binary/3,
  ws_close/3,
  ws_close/4,
  ws_ping/3,
  ws_pong/3
]).

%% @doc
%% @end
cookie(Req, Name, Value, Opts) ->
  paris_req:updt(
    Req,
    cowboy_req:set_resp_cookie(
      Name, Value, Opts,
      paris_req:req(Req))).

%% @doc
%% @end
delete_cookie(Req, Name) ->
  cookie(Req, Name, <<"">>, [{max_age, 0}]).

%% @doc
%% <pre>
%% render(Type, [
%%   {template, my_template},
%%   {data, []},
%%   {headers, [...]},
%%   {status, 200}]).
%% </pre>
%% @end
render(stream, Options) ->
  case elists:keyfind(path, 1, Options, false) of
    false -> {500, [], []};
    Path -> 
      Mime = paris_utils:mime(Path),
      Headers = elists:merge_keylists(
          1, elists:keyfind(headers, 1, Options, []),
          [{<<"Content-Type">>, Mime}]),
      Offset = elists:keyfind(offset, 1, Options, 0),
      Size = elists:keyfind(size, 1, Options, filelib:file_size(Path) - Offset),
      Sendfile = fun (Socket, Transport) ->
          case Transport:sendfile(Socket, Path, Offset, Size) of
            {ok, _} -> ok;
            {error, closed} -> ok;
            {error, etimedout} -> ok
          end
      end,
      {stream, Headers, {Size, Sendfile}}
  end;
render(Type, Options) ->
  Template = elists:keyfind(template, 1, Options, '-'),
  Data = elists:keyfind(data, 1, Options, []),
  Headers = elists:keyfind(headers, 1, Options, []),
  Status = elists:keyfind(status, 1, Options, 200),
  render(Type, Template, Data, Headers, Status).

%% @doc
%% @end
redirect(Format, Data) when is_binary(Format), is_list(Data) ->
  redirect(binary_to_list(Format), Data);
%% @doc
%% @end
redirect(Format, Data) when is_list(Format), is_list(Data) ->
  redirect(io_lib:format(Format, Data)).
%% @doc
%% @end
redirect(Path) when is_atom(Path) ->
  redirect(paris_router:path(Path));
%% @doc
%% @end
redirect(Path) when is_list(Path) ->
  redirect(list_to_binary(Path));
%% @doc
%% @end
redirect(Path) when is_binary(Path) ->
  {302, [{<<"Location">>, Path}], []}.

%% @doc
%% @end
ws_terminate() -> ok.
%% @doc
%% @end
ws_ok(Req, State) -> {ok, Req, State}.
%% @doc
%% @end
ws_ok(Req, State, Timeout) -> {ok, Req, State, Timeout}.
%% @doc
%% @end
ws_hibernate(Req, State) -> {ok, Req, State, hibernate}.
%% @doc
%% @end
ws_hibernate(Req, State, Timeout) -> {ok, Req, State, Timeout, hibernate}.
%% @doc
%% @end
ws_shutdown(Req, _) -> {shutdown, Req}.
%% @doc
%% @end
ws_text(Req, State, Msg) -> {reply, {text, Msg}, Req, State}.
%% @doc
%% @end
ws_json(Req, State, Msg) -> {reply, {text, jsx:encode(Msg)}, Req, State}.
%% @doc
%% @end
ws_binary(Req, State, Msg) -> {reply, {binary, Msg}, Req, State}.
%% @doc
%% @end
ws_close(Req, State, Msg) -> {reply, {close, Msg}, Req, State}.
%% @doc
%% @end
ws_close(Req, State, Msg, Code) -> {reply, {close, Code, Msg}, Req, State}.
%% @doc
%% @end
ws_ping(Req, State, Msg) -> {reply, {ping, Msg}, Req, State}.
%% @doc
%% @end
ws_pong(Req, State, Msg) -> {reply, {pong, Msg}, Req, State}.

%% @equiv http_error(Code, [])
http_error(Code) -> http_error(Code, []).
%% @doc
%% @end
http_error(Code, Headers) -> {Code, Headers, []}.
%% @doc
%% @end
http_error(Code, Headers, Body) -> {Code, Headers, Body}.

% private

render(html, '-', Data, Headers, Status) when is_list(Data) ->
  render(html, '-', list_to_binary(Data), Headers, Status);
render(html, '-', Data, Headers, Status) when is_binary(Data) ->
  render(
    inline, '-', Data, 
    elists:merge_keylists(1, Headers, [{<<"Content-Type">>, <<"text/html">>}]),
    Status);
render(xml, '-', Data, Headers, Status) when is_list(Data) ->
  render(xml, '-', exml:export(Data), Headers, Status);
render(xml, '-', Data, Headers, Status) when is_binary(Data) ->
  render(
    inline, '-', Data, 
    elists:merge_keylists(1, Headers, [{<<"Content-Type">>, <<"text/xml">>}]),
    Status);
render(text, '-', Data, Headers, Status) when is_list(Data) ->
  render(text, '-', list_to_binary(Data), Headers, Status);
render(text, '-', Data, Headers, Status) when is_binary(Data) ->
  render(
    inline, '-', Data,
    elists:merge_keylists(1, Headers, [{<<"Content-Type">>, <<"text/plain">>}]),
    Status);
render(json, '-', Data, Headers, Status) ->
  render(
    inline, '-', jsx:encode(Data),
    elists:merge_keylists(1, Headers, [{<<"Content-Type">>, <<"application/json">>}]),
    Status);
render(inline, '-', Data, Headers, Status) ->
  {Status, Headers, Data};

render(html, Template, Data, Headers, Status) ->
  render(
    inline, list_to_atom(atom_to_list(Template) ++ "_html"), Data,
    elists:merge_keylists(1, Headers, [{<<"Content-Type">>, <<"text/html">>}]),
    Status);
render(xml, Template, Data, Headers, Status) ->
  render(
    inline, list_to_atom(atom_to_list(Template) ++ "_xml"), Data,
    elists:merge_keylists(1, Headers, [{<<"Content-Type">>, <<"text/xml">>}]),
    Status);
render(text, Template, Data, Headers, Status) ->
  render(
    inline, list_to_atom(atom_to_list(Template) ++ "_txt"), Data,
    elists:merge_keylists(1, Headers, [{<<"Content-Type">>, <<"text/plain">>}]),
    Status);
render(json, Template, Data, Headers, Status) ->
  render(
    inline, list_to_atom(atom_to_list(Template) ++ "_json"), Data,
    elists:merge_keylists(1, Headers, [{<<"Content-Type">>, <<"application/json">>}]),
    Status);
render(inline, Template, Data, Headers, Status) ->
  case code:ensure_loaded(Template) of
    {module, Template} ->
      case Template:render(Data) of
        {ok, IOList} -> {Status, Headers, IOList};
        _ -> {500, [], []}
      end;
    _ -> {500, [], []}
  end.

