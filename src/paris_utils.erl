-module(paris_utils).

-export([
  mime/1
]).

mime(File) ->
  [$.|Ext] = filename:extension(File),
  case mimetypes:extension(string:to_lower(Ext)) of
    [M|_] -> M;
    T -> T
  end.
