-module(paris_utils).

-export([
  readlines/1,
  mime/1
]).

readlines(FileName) ->
  case file:open(FileName, [read]) of
    {ok, Device} ->
      try get_all_lines(Device)
      after file:close(Device)
      end;
    _ -> error
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of
    eof  -> [];
    Line -> Line ++ get_all_lines(Device)
  end.

mime(File) ->
  [$.|Ext] = filename:extension(File),
  case mimetypes:extension(Ext) of
    [M|_] -> M;
    T -> T
  end.
