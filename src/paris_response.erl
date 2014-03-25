-module(paris_response).

-export([
  render_view/1,
  render_view/2,
  render_view/3,
  render_text/1,
  render_text/2,
  render_stream/1,
  render_stream/2,
  redirect/1,
  http_error/1,
  http_error/2,
  ws_terminate/0,
  ws_ok/2,
  ws_ok/3,
  ws_hibernate/2,
  ws_hibernate/3,
  ws_shutdown/2,
  ws_text/3,
  ws_binary/3,
  ws_close/3,
  ws_close/4,
  ws_ping/3,
  ws_pong/3
]).

redirect(Path) when is_list(Path) ->
  redirect(list_to_binary(Path));
redirect(Path) when is_binary(Path) ->
  {302, [{<<"Location">>, Path}], []}.

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

render_view(View) ->
  render_view(View, [], []).
render_view(View, Variables) ->
  render_view(View, Variables, []).
render_view(View, Variables, Headers) when is_atom(View), is_list(Variables), is_list(Headers) ->
  ViewModule = list_to_atom(atom_to_list(View) ++ "_dtl"),
  case code:ensure_loaded(ViewModule) of
    {module, ViewModule} ->
      case ViewModule:render(Variables) of
        {ok, IOList} -> {200, Headers, IOList};
        _ -> {500, [], []}
      end;
    _ -> {500, [], []}
  end.

render_stream(Path) ->
  render_stream(Path, []).
render_stream(Path, Headers) ->
  Size = filelib:file_size(Path),
  Mime = paris_utils:mime(Path),
  Headers1 = Headers ++ [
    {<<"Content-Type">>, Mime}, 
    {<<"Accept-Ranges">>, <<"bytes">>}
  ],
  Sendfile = fun (Socket, Transport) ->
      case Transport:sendfile(Socket, Path) of
        {ok, _} -> ok;
        {error, closed} -> ok;
        {error, etimedout} -> ok
      end
  end,
  {stream, Headers1, {Size, Sendfile}}.

ws_terminate() -> ok.
ws_ok(Req, State) -> {ok, Req, State}.
ws_ok(Req, State, Timeout) -> {ok, Req, State, Timeout}.
ws_hibernate(Req, State) -> {ok, Req, State, hibernate}.
ws_hibernate(Req, State, Timeout) -> {ok, Req, State, Timeout, hibernate}.
ws_shutdown(Req, _) -> {shutdown, Req}.
ws_text(Req, State, Msg) -> {reply, {text, Msg}, Req, State}.
ws_binary(Req, State, Msg) -> {reply, {binary, Msg}, Req, State}.
ws_close(Req, State, Msg) -> {reply, {close, Msg}, Req, State}.
ws_close(Req, State, Msg, Code) -> {reply, {close, Code, Msg}, Req, State}.
ws_ping(Req, State, Msg) -> {reply, {ping, Msg}, Req, State}.
ws_pong(Req, State, Msg) -> {reply, {pong, Msg}, Req, State}.
