-module(paris_response).

-export([
  render_view/1,
  render_view/2,
  render_view/3,
  render_text/1,
  render_text/2
]).

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
