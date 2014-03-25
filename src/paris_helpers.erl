-module(paris_helpers).

-export([static/1]).

static(File) when is_list(File) ->
  case is_string(File) of
    true -> filename:join([paris:priv_dir(), "static", File]);
    false -> filename:join([paris:priv_dir(), "static"] ++ File)
  end.

is_string(X) ->
  lists:all(fun is_integer/1, X). 

