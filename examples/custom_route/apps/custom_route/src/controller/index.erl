-module(index).

-export([
  get/1,
  post/1,
  put/1,
  head/1,
  delete/1
]).

get(_Request) ->
  paris_response:render_view(index).

post(_Request) ->
  paris_response:render_view(index).

put(_Request) ->
  paris_response:render_view(index).

head(_Request) ->
  paris_response:render_view(index).

delete(_Request) ->
  paris_response:render_view(index).
