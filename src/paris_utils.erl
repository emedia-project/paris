%% @hidden
-module(paris_utils).

-export([
  mime/1,
  to_qs/1,
  urlencode/1,
  urldecode/1
]).

mime(File) ->
  [$.|Ext] = filename:extension(File),
  case mimetypes:extension(string:to_lower(Ext)) of
    [M|_] -> M;
    T -> T
  end.

to_qs(Params) when is_list(Params) ->
  to_qs(Params, <<"">>).
to_qs([], Qs) -> Qs;
to_qs([{Param, Value}|Rest], <<"">>) -> 
  to_qs(Rest, <<"?", (urlencode(eutils:to_binary(Param)))/binary, "=", (urlencode(eutils:to_binary(Value)))/binary>>);
to_qs([{Param, Value}|Rest], Qs) -> 
  to_qs(Rest, <<Qs/binary, "&", (urlencode(eutils:to_binary(Param)))/binary, "=", (urlencode(eutils:to_binary(Value)))/binary>>).

urlencode(B) when is_binary(B) ->
  cow_qs:urlencode(B);
urlencode(L) when is_list(L) ->
  binary_to_list(urlencode(list_to_binary(L))).

urldecode(B) when is_binary(B) ->
  cow_qs:urldecode(B);
urldecode(L) when is_list(L) ->
  binary_to_list(urldecode(list_to_binary(L))).

