-module(my).

-export([
  get/1,
  post/1,
  put/1,
  head/1,
  delete/1
]).

get(_Request) ->
  paris_response:render(html, [{template, my}]).

post(_Request) ->
  paris_response:render(html, [{template, my}]).

put(_Request) ->
  paris_response:render(html, [{template, my}]).

head(_Request) ->
  paris_response:render(html, [{template, my}]).

delete(_Request) ->
  paris_response:render(html, [{template, my}]).
