-module(paris_response).

-export([
  render/2,
  redirect/1,
  redirect/2,
  http_error/1,
  http_error/2,
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
  ws_pong/3,

  % Deprecated
  render_view/1,
  render_view/2,
  render_view/3,
  render_text/1,
  render_text/2,
  render_json/1,
  render_json/2,
  render_stream/1,
  render_stream/2,
  render_stream/3,
  render_stream/4
]).

%% render(Type, [
%%   {template, my_template},
%%   {data, []},
%%   {headers, [...]},
%%   {status, 200}]).
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

render(html, '-', Data, Headers, Status) when is_list(Data) ->
  render(html, '-', list_to_binary(Data), Headers, Status);
render(html, '-', Data, Headers, Status) when is_binary(Data) ->
  render(
    inline, '-', Data, 
    elists:merge_keylists(1, Headers, [{<<"Content-Type">>, <<"text/html">>}]),
    Status);
render(xml, '-', Data, Headers, Status) when is_list(Data) ->
  render(xml, '-', list_to_binary(Data), Headers, Status);
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

redirect(Format, Data) when is_list(Format), is_list(Data) ->
  redirect(io_lib:format(Format, Data)).
redirect(Path) when is_list(Path) ->
  redirect(list_to_binary(Path));
redirect(Path) when is_binary(Path) ->
  {302, [{<<"Location">>, Path}], []}.

ws_terminate() -> ok.
ws_ok(Req, State) -> {ok, Req, State}.
ws_ok(Req, State, Timeout) -> {ok, Req, State, Timeout}.
ws_hibernate(Req, State) -> {ok, Req, State, hibernate}.
ws_hibernate(Req, State, Timeout) -> {ok, Req, State, Timeout, hibernate}.
ws_shutdown(Req, _) -> {shutdown, Req}.
ws_text(Req, State, Msg) -> {reply, {text, Msg}, Req, State}.
ws_json(Req, State, Msg) -> {reply, {text, jsx:encode(Msg)}, Req, State}.
ws_binary(Req, State, Msg) -> {reply, {binary, Msg}, Req, State}.
ws_close(Req, State, Msg) -> {reply, {close, Msg}, Req, State}.
ws_close(Req, State, Msg, Code) -> {reply, {close, Code, Msg}, Req, State}.
ws_ping(Req, State, Msg) -> {reply, {ping, Msg}, Req, State}.
ws_pong(Req, State, Msg) -> {reply, {pong, Msg}, Req, State}.

% Deprecated ------------------------------------------------------------------

http_error(Code) -> http_error(Code, []).
http_error(Code, Headers) -> {Code, Headers, []}.

render_text(Data) when is_list(Data) ->
  render_text(list_to_binary(Data), []);
render_text(Data) when is_binary(Data) ->
  render_text(Data, []).
render_text(Data, Headers) when is_list(Data), is_list(Headers) ->
  render_text(list_to_binary(Data), Headers);
render_text(Data, Headers) when is_binary(Data), is_list(Headers) ->
  {200, Headers ++ [{<<"Content-Type">>, <<"text/plain">>}], Data}. 

render_json(Data) ->
  render_json(Data, []).
render_json(Data, Headers) ->
  {200, Headers ++ [{<<"Content-Type">>, <<"application/json">>}], jsx:encode(Data)}.

render_view(View) ->
  render_view(View, [], []).
render_view(View, Variables) ->
  render_view(View, Variables, []).
render_view(View, Variables, Headers) when is_atom(View), is_list(Variables), is_list(Headers) ->
  ViewModule = list_to_atom(atom_to_list(View) ++ "_html"),
  case code:ensure_loaded(ViewModule) of
    {module, ViewModule} ->
      case ViewModule:render(Variables) of
        {ok, IOList} -> {200, Headers, IOList};
        _ -> {500, [], []}
      end;
    _ -> {500, [], []}
  end.

render_stream(Path) ->
  render_stream(Path, 0, filelib:file_size(Path), []).
render_stream(Path, Offset) when is_integer(Offset) ->
  render_stream(Path, Offset, filelib:file_size(Path) - Offset, []);
render_stream(Path, Headers) when is_list(Headers) ->
  render_stream(Path, 0, filelib:file_size(Path), Headers).
render_stream(Path, Offset, Size) when is_integer(Size) ->
  render_stream(Path, Offset, Size, []);
render_stream(Path, Offset, Headers) when is_list(Headers) ->
  render_stream(Path, Offset, filelib:file_size(Path) - Offset, Headers).
render_stream(Path, Offset, Size, Headers) ->
  Mime = paris_utils:mime(Path),
  Headers1 = Headers ++ [
    {<<"Content-Type">>, Mime} 
  ],
  Sendfile = fun (Socket, Transport) ->
      case Transport:sendfile(Socket, Path, Offset, Size) of
        {ok, _} -> ok;
        {error, closed} -> ok;
        {error, etimedout} -> ok
      end
  end,
  {stream, Headers1, {Size, Sendfile}}.

