-module(paris).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([
  priv_dir/0
]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

priv_dir() ->
  gen_server:call(?SERVER, {priv_dir}).

start_link(Args) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).
init(Args) ->
  {ok, Args}.

handle_call({priv_dir}, _From, State) ->
  PrivDir = case lists:keyfind(app, 1, State) of
    false -> error;
    {app, App} -> code:priv_dir(App)
  end,
  {reply, PrivDir, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

