%% @hidden
-module(paris_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Args) ->
  AppName = elists:keyfind(app, 1, Args),
  GetTextDir = code:priv_dir(AppName),
  DefaultLang = application:get_env(AppName, i18n, "en"),
  {ok, {{one_for_one, 5, 10}, [
        ?CHILD(paris, worker, [Args]),
        ?CHILD(paris_i18n, worker, [[{gettext_dir, GetTextDir}, {default_lang, DefaultLang}]])
      ]}}.
