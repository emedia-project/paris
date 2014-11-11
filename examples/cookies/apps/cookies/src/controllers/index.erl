-module(index).

-export([
  all/1
]).

all(Request) ->
  NewValue = integer_to_list(random:uniform(1000000)),
  paris_response:cookie(Request, <<"cookie">>, NewValue, [{path, <<"/">>}]),
  Value = paris_request:cookie(Request, <<"cookie">>),
  paris_response:render(
    html, 
    [{template, index}, {data, [{value, Value}, {next, NewValue}]}]).


