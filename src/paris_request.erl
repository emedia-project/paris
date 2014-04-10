-module(paris_request).

-export([
  body/1,
  content_type/1,
  content_type/2,
  accept/1,
  accept/2
]).

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

