-module(paris_redirect).

-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Request, _Opts) ->
  {ok, Request, _Opts}.

handle(Request, State) ->
  [DestRoute, Params] = State,
  Params2 = paris_request:params(Request, get),
  Funs = [
          {1, fun eutils:to_list/1},
          {2, fun eutils:to_list/1}
         ],
  FinalRoute = DestRoute ++ paris_utils:to_qs(
                              elists:merge_keylists(
                                1, 
                                elists:keylistmap(Funs, Params),
                                elists:keylistmap(Funs, Params2)
                               )),
  {ok, R} = cowboy_req:reply(302, [{<<"Location">>, list_to_binary(FinalRoute)}], [], Request),
  {ok, R, State}.

terminate(_Req, _State, _) ->
  ok.
