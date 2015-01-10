% @hidden
-module(paris_req).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
         start_link/1, 
         req/1,
         updt/2,
         stop/1
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(CowboyReq) ->
  gen_server:start_link(?MODULE, CowboyReq, []).

req(Pid) ->
  gen_server:call(Pid, req).
updt(Pid, Req) ->
  gen_server:call(Pid, {updt, Req}).
stop(Pid) ->
  gen_server:call(Pid, stop).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  {ok, Args}.

handle_call(req, _From, State) ->
  {reply, State, State};
handle_call({updt, Req}, _From, _State) ->
  {reply, ok, Req};
handle_call(stop, _From, State) ->
  {stop, normal, shutdown_ok, State};
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

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

