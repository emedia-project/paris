-module(my).

-export([
  get/1,
  post/1,
  put/1,
  head/1,
  delete/1
]).

get(_Request) ->
  paris_response:render_view(my).

post(_Request) ->
  paris_response:render_view(my).

put(_Request) ->
  paris_response:render_view(my).

head(_Request) ->
  paris_response:render_view(my).

delete(_Request) ->
  paris_response:render_view(my).
