-module(websocket).

-behaviour(paris_websocket).

-export([
  init/3,
  handle/3,
  info/3,
  terminate/3
]).

init(_TransportName, Req, State) ->
  gproc:reg({p, l, my_room}),
  paris_response:ws_ok(Req, State).

handle({text, Msg}, Req, State) ->
  gproc:send({p, l, my_room}, Msg),
  paris_response:ws_ok(Req, State);
handle(_, Req, State) ->
  paris_response:ws_ok(Req, State).

info(Msg, Req, State) ->
  paris_response:ws_text(Req, State, Msg).

terminate(_, _, _) ->
  paris_response:ws_terminate().
