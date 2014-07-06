-module(index).

-export([
  get/1
]).

get(_Request) ->
  DB = paris:get(db),
  Posts = lists:map(fun(P) ->
          P:to_keylist()
      end, posts:find(DB, all, [])),
  paris_response:render_view(index, [{posts, Posts}]).

