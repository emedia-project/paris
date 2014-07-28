-module(my_route).

-export([
  get/1,
  post/1,
  put/1,
  head/1,
  delete/1
]).

get(_Request) ->
  paris_response:render(html, [{template, my_route}]).

post(_Request) ->
  paris_response:render(html, [{template, my_route}]).

put(_Request) ->
  paris_response:render(html, [{template, my_route}]).

head(_Request) ->
  paris_response:render(html, [{template, my_route}]).

delete(_Request) ->
  paris_response:render(html, [{template, my_route}]).
