%% @doc
%% tests (WIP)
%% @end
-module(paris_test).

-include_lib("eunit/include/eunit.hrl").

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
         assert_code/2,
         assert_not_code/2,
         assert_has_body/1,
         assert_not_has_body/1,
         assert_redirect/1,
         assert_not_found/1,
         assert_header/3
        ]).

-export([
         log/1,
         log/2
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
  hackney:start(),
  os:putenv("PARIS_RUN_MODE", "production"),
  AppName:start().

request(Method, URL, Fun) ->
  request(Method, URL, [], <<>>, [], Fun).

request(Method, URL, Headers, Body, Options, Fun) when is_atom(Method),
                                                       is_binary(URL),
                                                       is_list(Headers),
                                                       is_binary(Body),
                                                       is_list(Options),
                                                       is_function(Fun) ->
  Fun(case hackney:request(Method, eutils:to_binary(URL), Headers, eutils:to_binary(Body), Options) of
        {ok, StatusCode, RespHeaders, ClientRef} ->
          case hackney:body(ClientRef) of
            {ok, RespBody} ->
              {ok, StatusCode, RespHeaders, RespBody};
            E -> 
              E
          end;
        {ok, StatusCode, RespHeaders} ->
          {ok, StatusCode, RespHeaders, <<>>};
        E ->
          E
      end).

follow(Method, URL, Fun) ->
  case http_uri:parse(eutils:to_string(URL)) of
    {ok, {Scheme, UserInfo, Host, Port, _, _}} ->
      Base = eutils:to_binary(
               case UserInfo of
                 [] -> eutils:to_string(Scheme) ++ "://" ++ 
                       Host ++ ":" ++ eutils:to_string(Port);
                 _ -> eutils:to_string(Scheme) ++ "://" ++ 
                      UserInfo ++ "@" ++ 
                      Host ++ ":" ++ eutils:to_string(Port)
               end),
      request(Method, URL, fun({ok, StatusCode, RespHeaders, <<>>}) when StatusCode >= 300, StatusCode < 400 ->
                               case get_header(<<"Location">>, RespHeaders) of
                                 <<>> -> Fun({error, not_redirect});
                                 Location -> request(get, <<Base/binary, Location/binary>>, Fun)
                               end;
                              (X) ->
                               Fun(X)
                           end);
    E ->
      Fun(E)
  end.

submit(FormName, FormValue, Assertions, Continuations) ->
  todo.

assert_ok({ok, Code, _, _}) ->
  ?assert(Code >= 200 andalso Code < 300);
assert_ok(_) ->
  ?assert(false).

assert_code({ok, Code, _, _}, ExpectedCode) ->
  ?assert(Code =:= ExpectedCode);
assert_code(_, _) ->
  ?assert(false).

assert_not_code({ok, Code, _, _}, ExpectedCode) ->
  ?assert(Code =/= ExpectedCode);
assert_not_code(_, _) ->
  ?assert(false).

assert_has_body({ok, _, _, Body}) ->
  ?assert(<<>> =/= Body);
assert_has_body(_) ->
  ?assert(false).

assert_not_has_body({ok, _, _, Body}) ->
  ?assert(<<>> =:= Body);
assert_not_has_body(_) ->
  ?assert(false).

assert_redirect({ok, Code, _, _}) ->
  ?assert(Code >= 300 andalso Code < 400);
assert_redirect(_) ->
  ?assert(false).

assert_not_found({ok, 404, _, _}) ->
  ?assert(true);
assert_not_found(_) ->
  ?assert(false).

assert_header({ok, _, Headers, _}, Header, Value) ->
  ?assert(get_header(Header, Headers) =:= eutils:to_binary(Value));
assert_header(_, _, _) ->
  ?assert(false).

log(Fmt, Data) ->
  ?debugFmt(Fmt, Data).

log(Data) ->
  ?debugMsg(Data).

get_header(Header, Headers) ->
  elists:keyfind(eutils:to_binary(Header), 1, Headers, <<>>).
