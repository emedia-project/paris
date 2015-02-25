%% @doc
%% tests (WIP)
%% @end
-module(paris_test).

-export([
         start/1
        ]).

% Helpers
-export([
         request/3,
         request/6,
         follow/3,
         submit/4
        ]).

% Assert
-export([
         assert_ok/1,
         assert_redirect/1,
         assert_not_found/1,
         assert_header/3
        ]).

-define(HTTP_OPTIONS, [{timeout, infinity},
                       {connect_timeout, infinity},
                       {autoredirect, false},
                       {version, "HTTP/1.1"}]).
-define(OPTIONS, [{sync, true},
                  {full_result, true}]).

%% Example
%% <pre>
%% paris_test:request(get, "http://localhost:8080", [], "", [], fun(Response) -> ... end)
%% </pre>
%%
%% {timeout, timeout()} | {connect_timeout, timeout()} | {ssl, ssloptions()} | {essl, ssloptions()} | {autoredirect, boolean()} | {proxy_auth, {userstring(), passwordstring()}} | {version, http_version()} | {relaxed, boolean()} | {url_encode, boolean()}
%% {sync, boolean()} | {stream, stream_to()} | {body_format, body_format()} | {full_result, boolean()} | {headers_as_is, boolean() | {socket_opts, socket_opts()} | {receiver, receiver()}, {ipv6_host_with_brackets, boolean()}}

start(AppName) ->
  os:putenv("PARIS_RUN_MODE", "production"),
  AppName:start().

request(Mode, Url, Fun) ->
  request(Mode, Url, [], "", [], Fun).

request(Mode, Url, Headers, Body, Options, Fun) ->
  todo.

follow(LinkOrRedir, Assertions, Continuations) ->
  todo.

submit(FormName, FormValue, Assertions, Continuations) ->
  todo.

assert_ok(Response) ->
  todo.

assert_redirect(Response) ->
  todo.

assert_not_found(Response) ->
  todo.

assert_header(Response, Header, Value) ->
  todo.

