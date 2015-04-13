-module(paris_i18n).

-export([
         start_link/1, 
         lang/1,
         lang/0,
         trans/1
        ]).

start_link(Args) ->
  R = gettext_server:start_link({gettext_server, Args}),
  gettext:recreate_db(gettext_server),
  R.

lang(Lang) ->
  paris:set(gettext_language, eutils:to_string(Lang)).

lang() ->
  paris:get(gettext_language).

trans(Data) ->
  gettext:key2str(gettext_server, eutils:to_string(Data), lang()).
