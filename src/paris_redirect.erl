% @hidden
-module(paris_redirect).

-export([init/2]).

init(Request, Opts) ->
  handle(Request, Opts).

handle(Request, State) ->
  [DestRoute, Params] = State,
  Params2 = paris_request:params(Request, get),
  Funs = [
          {1, fun eutils:to_binary/1},
          {2, fun eutils:to_binary/1}
         ],
  FinalRoute = <<(eutils:to_binary(DestRoute))/binary,
                 (paris_utils:to_qs(
                              elists:merge_keylists(
                                1, 
                                elists:keylistmap(Funs, Params2),
                                elists:keylistmap(Funs, Params)
                               )))/binary>>,
  lager:info("REDIRECT ~s", [FinalRoute]),
  {ok, 
   cowboy_req:reply(302, [{<<"Location">>, FinalRoute}], Request), 
   State}.

