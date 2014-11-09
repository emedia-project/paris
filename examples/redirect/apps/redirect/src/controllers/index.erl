-module(index).

-export([
  get/1,
  post/1,
  put/1,
  head/1,
  delete/1
]).

get(Request) ->
  Params = paris_request:params(Request),
  paris_response:render(html, [{template, index}, {data, Params}]).

post(_Request) ->
  paris_response:render(html, [{template, index}]).

put(_Request) ->
  paris_response:render(html, [{template, index}]).

head(_Request) ->
  paris_response:render(html, [{template, index}]).

delete(_Request) ->
  paris_response:render(html, [{template, index}]).
