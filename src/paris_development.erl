%% @hidden
-module(paris_development).

-export([start/0]).

start() ->
  application:start(sync).
