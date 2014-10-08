-module(paris_utils).

-export([
  mime/1,
  to_qs/1
]).

mime(File) ->
  [$.|Ext] = filename:extension(File),
  case mimetypes:extension(string:to_lower(Ext)) of
    [M|_] -> M;
    T -> T
  end.

to_qs(Params) when is_list(Params) ->
  to_qs(Params, "").
to_qs([], Qs) -> lists:flatten(Qs);
to_qs([{Param, Value}|Rest], "") -> 
  to_qs(Rest, "?" ++ Param ++ "=" ++ Value);
to_qs([{Param, Value}|Rest], Qs) -> 
  to_qs(Rest, Qs ++ "&" ++ Param ++ "=" ++ Value).

