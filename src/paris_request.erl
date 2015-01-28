-module(paris_request).

-export([
  peer/1,
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
  headers/1,
  method/1,
  cookies/1,
  cookie/2
]).

%% @doc
%% @end
peer(Req) ->
  cowboy_req:peer(paris_req:req(Req)).

%% @doc
%% @end
cookies(Req) ->
  cowboy_req:parse_cookies(paris_req:req(Req)).

%% @doc
%% @end
cookie(Req, Name) ->
  elists:keyfind(Name, 1, cookies(Req), undefined).

%% @doc
%% @end
method(Req) ->
  cowboy_req:method(paris_req:req(Req)).

%% @doc
%% @end
header(Req, Name) ->
  cowboy_req:header(Name, paris_req:req(Req)).

%% @doc
%% @end
headers(Req) ->
  cowboy_req:headers(paris_req:req(Req)).

%% @doc
%% @end
param(Req, Type, Name) ->
  case lists:keyfind(Name, 1, params(Req, Type)) of
    {Name, Value} -> Value;
    _ -> undefined
  end.

%% @doc
%% @end
param(Req, Name) ->
  case lists:keyfind(Name, 1, params(Req)) of
    {Name, Value} -> Value;
    _ -> undefined
  end.

%% @doc
%% @end
params(Req, Type) ->
  case Type of
    get -> get_vals(Req);
    post -> post_vals(Req);
    bind -> binding_vals(Req)
  end.

%% @doc
%% @end
params(Req) ->
  merge_params_array(get_vals(Req) ++ post_vals(Req) ++ binding_vals(Req)).

%% @doc
%% @end
body(Req) ->
  case cowboy_req:body(paris_req:req(Req)) of
    {ok, Data, _Req2} -> {ok, Data};
    E -> E
  end.

%% @doc
%% @end
content_type(Req) ->
  cowboy_req:parse_header(<<"content-type">>, paris_req:req(Req), undefined).

%% @doc
%% @end
content_type(Req, ContentType) ->
  [Type, SubType | _] = binary:split(ContentType, <<"/">>),
  case content_type(Req) of
    {Type2, SubType2, _} ->
      (Type =:= Type2 orelse <<"*">> =:= Type2) and
      (SubType =:= SubType2 orelse <<"*">> =:= SubType2);
    _ -> false
  end.

%% @doc
%% @end
accept(Req) ->
  cowboy_req:parse_header(<<"accept">>, paris_req:req(Req), []).

%% @doc
%% @end
accept(Req, Accept) ->
  [Type, SubType | _] = binary:split(Accept, <<"/">>),
  lists:any(fun({{Type2, SubType2, _}, _, _}) ->
        (Type =:= Type2 orelse <<"*">> =:= Type2) and
        (SubType =:= SubType2 orelse <<"*">> =:= SubType2)
    end, accept(Req)).

%% @doc
%% @end
range(Req) ->
  Range = cowboy_req:header(<<"range">>, paris_req:req(Req), <<"bytes=0-">>),
  [_, Range1|_] = string:tokens(binary_to_list(Range), "="),
  list_to_tuple([trunc(list_to_integer(X)/1024) || 
      X <- string:tokens(Range1, "-")]).

% private

post_vals(Req) ->
  case cowboy_req:body_qs(paris_req:req(Req)) of
    {ok, List, _} -> merge_params_array(List);
    _ -> []
  end.

get_vals(Req) ->
  merge_params_array(cowboy_req:parse_qs(paris_req:req(Req))).

binding_vals(Req) ->
  merge_params_array(cowboy_req:bindings(paris_req:req(Req))).

merge_params_array(Params) ->
  lists:foldl(fun({Key, Value}, Acc) ->
                  RealKey = case re:run(Key, "([^\\[]*)\\[\\]$",[{capture,[1],list}]) of
                              {match, [Key1]} -> eutils:to_binary(Key1);
                              nomatch -> Key
                            end,
                  case lists:keyfind(RealKey, 1, Acc) of
                    {RealKey, CurrentValue} when is_list(CurrentValue) ->
                      lists:keyreplace(RealKey, 1, Acc, {RealKey, lists:flatten([value_to_list(Value)|CurrentValue])});
                    {RealKey, CurrentValue} ->
                      lists:keyreplace(RealKey, 1, Acc, {RealKey, lists:flatten([value_to_list(Value), CurrentValue])});
                    false ->
                      [{RealKey, value_to_list(Value)}|Acc]
                  end
              end, [], Params).

value_to_list(Value) ->
  case re:run(Value, "\\[([^\\]]*)]$",[{capture,[1],list}]) of
    {match, [Value1]} -> [eutils:to_binary(X) || X <- string:tokens(eutils:to_string(Value1), ",")];
    nomatch -> Value
  end.
