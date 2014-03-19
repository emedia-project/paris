-module(paris_helpers).

-export([static/1]).

static(File) ->
  filename:join([paris:priv_dir(), "static", File]).

