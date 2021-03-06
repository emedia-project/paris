-module(paris_request).

-export([
  body/1,
  content_type/1,
  content_type/2,
  accept/1,
  accept/2,
  range/1,
  param/3,
  param/2,
  params/2,
  params/1,
  header/2,
  headers/1
]).

header(Req, Name) ->
  {Value, _} = cowboy_req:header(Name, Req),
  Value.

headers(Req) ->
  {Headers, _} = cowboy_req:headers(Req),
  Headers.

param(Req, Type, Name) ->
  case lists:keyfind(Name, 1, params(Req, Type)) of
    {Name, Value} -> Value;
    _ -> undefined
  end.

param(Req, Name) ->
  case lists:keyfind(Name, 1, params(Req)) of
    {Name, Value} -> Value;
    _ -> undefined
  end.

params(Req, Type) ->
  case Type of
    get -> get_vals(Req);
    post -> post_vals(Req)
  end.

params(Req) ->
  get_vals(Req) ++ post_vals(Req).

post_vals(Req) ->
  case cowboy_req:body_qs(Req) of
    {ok, List, _} -> List;
    _ -> []
  end.
get_vals(Req) ->
  {Params2, _} = cowboy_req:qs_vals(Req),
  Params2.

body(Req) ->
  case cowboy_req:body(Req) of
    {ok, Data, _Req2} -> {ok, Data};
    E -> E
  end.

content_type(Req) ->
  case cowboy_req:parse_header(<<"content-type">>, Req) of
    {ok, ParsedValue, _} -> ParsedValue;
    _ -> undefined
  end.

content_type(Req, ContentType) ->
  [Type, SubType | _] = binary:split(ContentType, <<"/">>),
  case content_type(Req) of
    {Type2, SubType2, _} ->
      (Type =:= Type2 orelse <<"*">> =:= Type2) and
      (SubType =:= SubType2 orelse <<"*">> =:= SubType2);
    _ -> false
  end.

accept(Req) ->
  case cowboy_req:parse_header(<<"accept">>, Req) of
    {ok, ParsedValue, _} -> ParsedValue;
    _ -> []
  end.

accept(Req, Accept) ->
  [Type, SubType | _] = binary:split(Accept, <<"/">>),
  lists:any(fun({{Type2, SubType2, _}, _, _}) ->
        (Type =:= Type2 orelse <<"*">> =:= Type2) and
        (SubType =:= SubType2 orelse <<"*">> =:= SubType2)
    end, accept(Req)).

range(Req) ->
  {Range, _} = cowboy_req:header(<<"range">>, Req, <<"bytes=0-">>),
  [_, Range1|_] = string:tokens(binary_to_list(Range), "="),
  list_to_tuple([trunc(list_to_integer(X)/1024) || 
      X <- string:tokens(Range1, "-")]).
