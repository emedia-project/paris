-module(my_route_two).

-export([
  get/1,
  get/2,
  post/1,
  put/1,
  head/1,
  delete/1
]).

get(_Request) ->
  get(_Request, null).
get(_Request, _) ->
  paris_response:render(html, [{template, my_route_two}]).

post(_Request) ->
  paris_response:render(html, [{template, my_route_two}]).

put(_Request) ->
  paris_response:render(html, [{template, my_route_two}]).

head(_Request) ->
  paris_response:render(html, [{template, my_route_two}]).

delete(_Request) ->
  paris_response:render(html, [{template, my_route_two}]).
