-module(index).

-export([
  get/1
]).

get(_Request) ->
  paris_response:redirect("/index.html").

