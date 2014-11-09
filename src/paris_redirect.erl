-module(paris_redirect).

-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Request, _Opts) ->
  {ok, Request, _Opts}.

handle(Request, State) ->
  [DestRoute, Params] = State,
  Params2 = paris_request:params(Request, get),
  FinalRoute = DestRoute ++ paris_utils:to_qs(Params2 ++ Params),
  {ok, 
   cowboy_req:reply(302, [{<<"Location">>, list_to_binary(FinalRoute)}], [], Request), 
   State}.

terminate(_Req, _State, _) ->
  ok.
