-module(video).

-export([
    get/2
  ]).

get(_Request, Format) ->
  File = paris:static("videos/demo." ++ Format),
  paris_response:render_stream(File).
