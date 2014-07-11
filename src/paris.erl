-module(paris).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([
  priv_dir/0,
  static/1,
  port/0,
  app/0,
  plugins/0,
  set/2,
  get/1,
  del/1
]).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

priv_dir() ->
  gen_server:call(?SERVER, priv_dir).

static(File) when is_list(File) ->
  case is_string(File) of
    true -> filename:join([priv_dir(), "static", File]);
    false -> filename:join([priv_dir(), "static"] ++ File)
  end.

port() ->
  gen_server:call(?SERVER, port).

app() ->
  gen_server:call(?SERVER, app).

plugins() ->
  gen_server:call(?SERVER, plugins).

set(Name, Value) -> 
  gen_server:call(?SERVER, {set, Name, Value}).

get(Name) ->
  gen_server:call(?SERVER, {get, Name}).

del(Name) ->
  gen_server:call(?SERVER, {del, Name}).

start_link(Args) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).
init(Args) ->
  State = case lists:keyfind(app, 1, Args) of
    false -> Args;
    {app, A} -> 
      case application:get_env(A, plugins) of
        {ok, Plugins} -> Args ++ [{plugins, Plugins}];
        _ -> Args ++ [{plugins, []}]
      end
  end,
  {ok, State ++ [{user, []}]}.

handle_call(priv_dir, _From, State) ->
  PrivDir = case lists:keyfind(app, 1, State) of
    false -> error;
    {app, App} -> code:priv_dir(App)
  end,
  {reply, PrivDir, State};
handle_call(port, _From, State) ->
  Port = case lists:keyfind(port, 1, State) of
    false -> error;
    {port, P} -> P
  end,
  {reply, Port, State};
handle_call(app, _From, State) ->
  Port = case lists:keyfind(app, 1, State) of
    false -> error;
    {app, A} -> A
  end,
  {reply, Port, State};
handle_call(plugins, _From, State) ->
  Port = case lists:keyfind(plugins, 1, State) of
    false -> error;
    {plugins, A} -> A
  end,
  {reply, Port, State};
handle_call({set, Name, Value}, _From, State) ->
  {Result, State1} = case lists:keyfind(user, 1, State) of
    false -> {error, State};
    {user, UserData} ->
      UserData1 = elists:merge_keylists(1, [{Name, Value}], UserData),
      State2 = elists:merge_keylists(1, [{user, UserData1}], State),
      {ok, State2}
  end,
  {reply, Result, State1};
handle_call({get, Name}, _From, State) ->
  Value = case lists:keyfind(user, 1, State) of
    false -> undefined;
    {user, UserData} ->
      case lists:keyfind(Name, 1, UserData) of
        false -> undefined;
        {Name, Value1} -> Value1
      end
  end,
  {reply, Value, State};
handle_call({del, Name}, _From, State) ->
  {Result, State1} = case lists:keyfind(user, 1, State) of
    false -> {error, State};
    {user, UserData} ->
      UserData1 = lists:keydelete(Name, 1, UserData),
      State2 = elists:merge_keylists(1, [{user, UserData1}], State),
      {ok, State2}
  end,
  {reply, Result, State1};
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

% - private -

is_string(X) ->
  lists:all(fun is_integer/1, X). 

