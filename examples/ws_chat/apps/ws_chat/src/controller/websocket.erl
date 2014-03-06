-module(websocket).

-behaviour(paris_websocket).

-export([
  init/3,
  handle/3,
  info/3,
  terminate/3
]).

init(_, Req, _) ->
  gproc:reg({p, l, my_room}),
  {ok, Req, undefined_state}.

handle({text, Msg}, Req, State) ->
  gproc:send({p, l, my_room}, Msg),
  {ok, Req, State};
handle(_, Req, State) ->
  {ok, Req, State}.

info(Msg, Req, State) ->
  {reply, {text, Msg}, Req, State}.

terminate(_, _, _) ->
  ok.
