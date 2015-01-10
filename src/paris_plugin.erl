% @hidden
-module(paris_plugin).

-export([call/4]).

call(Type, Module, Action, Params) ->
  case paris:plugins() of
    error -> undef;
    [] -> undef;
    Plugins -> call(Plugins, Type, [Module, Action, Params])
  end.

call([], _, _) -> undef;
call([Plugin|Plugins], Type, Params) ->
  case erlang:apply(Plugin, type, []) of
    Type -> 
      case erlang:apply(Plugin, call, Params) of
        {error, _} -> call(Plugins, Type, Params);
        {ok, R} -> R
      end;
    _ -> call(Plugins, Type, Params)
  end.

