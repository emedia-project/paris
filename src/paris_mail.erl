-module(mail).

-export([test1/0, test2/0]).

% send(
%   <<"gregoire.lejeune@gmail.com">>,
%   [{<<"To">, <<"gregoire.lejeune@free.fr">>},
%    {<<"Cc">, [<<"gregoire.lejeune@gmail.com">>, <<"lejeune.gregoire@me.com">>]}],
%   <<"Test gen_smtp">>,
%   [{text, <<"Contenu au format text">>},
%    {html, <<"Contenu au format <b>html</b>">>}],
%   [<<"/Users/glejeune/Dropbox/Greg/images/greg.jpg">>]).
send(From, Dest, Subject, Bodies, Attachments) ->
  Headers = [
      {<<"From">>, From}, 
      {<<"Subject">>, Subject},
      {<<"MIME-Version">>, <<"1.0">>},
      {<<"Message-ID">>, list_to_binary(smtp_util:generate_message_id())},
      {<<"Date">>, list_to_binary(smtp_util:rfc5322_timestamp())}
      ] ++ get_dests(Dest),
  HasAttachement = length(Attachments) > 0,
  MultipartBody = length(Bodies) > 1,
  Mail = mimemail:encode(gen_mail(HasAttachement, MultipartBody, Headers, Bodies, Attachments)),
  {binary_to_list(From), dests_list(Dest), Mail}.
  
dests_list(Dests) ->
  lists:foldl(fun({_, Dest}, Acc) ->
        case is_list(Dest) of
          false -> [binary_to_list(Dest)|Acc];
          true -> Acc ++ lists:map(fun binary_to_list/1, Dest)
        end
    end, [], Dests).

get_dests(Dests) ->
  lists:foldl(fun({Type, Dest}, Acc) ->
        if
          Type =:= <<"To">> orelse Type =:= <<"Cc">> ->
            case is_list(Dest) of
              true -> [{Type, lists:foldr(fun(A, B) -> <<A/binary, ",", B/binary>> end, <<>>, Dest)}|Acc];
              false -> [{Type, Dest}|Acc]
            end;
          true -> Acc
        end
    end, [], Dests).

gen_mail(false, false, Headers, [Body], _) ->
  gen_single(Headers, Body);
gen_mail(false, true, Headers, Bodies, _) ->
  gen_multipart_alternative(
    Headers,
    lists:map(fun gen_multipart_body/1, Bodies));
gen_mail(true, false, Headers, [Body], Attachments) ->
  gen_multipart_mixed(
    Headers,
    [gen_multipart_body(Body)] ++
    lists:map(fun gen_attachment/1, Attachments));
gen_mail(true, true, Headers, Bodies, Attachments) ->
  gen_multipart_mixed(
    Headers,
    [gen_multipart_alternative(lists:map(fun gen_multipart_body/1, Bodies))] ++
    lists:map(fun gen_attachment/1, Attachments)).


gen_single(Headers, {Type, Body}) ->
  case Type of
    text -> {
        <<"text">>, <<"plain">>, Headers, 
        [{<<"content-type-params">>,
          [{<<"charset">>,<<"UTF-8">>}],
          {<<"disposition">>,<<"inline">>}}],
        Body};
    html -> {
        <<"text">>,<<"html">>, Headers,
        [{<<"content-type-params">>,
          [{<<"charset">>,<<"UTF-8">>}],
          {<<"disposition">>,<<"inline">>}}],
        Body};
    _ -> error
  end.

gen_multipart_alternative(Bodies) -> 
  gen_multipart_alternative([], Bodies).
gen_multipart_alternative(Headers, Bodies) ->
  Boundary = list_to_binary(smtp_util:generate_message_boundary()),
  {<<"multipart">>, <<"alternative">>, 
   Headers ++ [{<<"Content-Type">>, <<"multipart/alternative; boundary=", Boundary/binary>>}],
   [{<<"content-type-params">>, [{<<"boundary">>, Boundary}]},
    {<<"disposition">>,<<"inline">>},
    {<<"disposition-params">>,[]}],
   Bodies}.

gen_multipart_mixed(Headers, Bodies) ->
  Boundary = list_to_binary(smtp_util:generate_message_boundary()),
  {<<"multipart">>, <<"mixed">>, 
   Headers ++ [{<<"Content-Type">>, <<"multipart/mixed; boundary=", Boundary/binary>>}],
   [{<<"content-type-params">>, [{<<"boundary">>, Boundary}]},
    {<<"disposition">>,<<"inline">>},
    {<<"disposition-params">>,[]}],
   Bodies}.

gen_multipart_body({text, Body}) ->
  {<<"text">>,<<"plain">>,
   [{<<"Content-Type">>, <<"text/plain;charset=UTF-8">>},
    {<<"Content-Transfer-Encoding">>,<<"quoted-printable">>}],
   [{<<"content-type-params">>, [{<<"charset">>,<<"UTF-8">>}]},
    {<<"disposition">>,<<"inline">>},
    {<<"disposition-params">>,[]}],
   Body};
gen_multipart_body({html, Body}) ->
  {<<"text">>,<<"html">>,
   [{<<"Content-Type">>,<<"text/html;charset=UTF-8">>},
    {<<"Content-Transfer-Encoding">>,<<"quoted-printable">>}],
   [{<<"content-type-params">>, [{<<"charset">>,<<"UTF-8">>}]},
    {<<"disposition">>,<<"inline">>},
    {<<"disposition-params">>,[]}],
   Body}.

gen_attachment(File) ->
  Basename = list_to_binary(filename:basename(binary_to_list(File))),
  {Type, SubType} = mimetype(File),
  {ok, Binary} = file:read_file(File),
  {Type, SubType,
   [{<<"Content-Disposition">>,<<"attachment;filename=", Basename/binary, "">>},
    {<<"Content-Type">>,
     <<Type/binary, "/", SubType/binary, ";x-unix-mode=0644;name=\"", Basename/binary, "\"">>},
    {<<"Content-Transfer-Encoding">>,<<"base64">>}],
   [{<<"content-type-params">>,
     [{<<"x-unix-mode">>,<<"0644">>},{<<"name">>,<<"", Basename/binary>>}]},
    {<<"disposition">>,<<"attachment">>},
    {<<"disposition-params">>,[{<<"filename">>,<<"", Basename/binary>>}]}],
   Binary}.

mimetype(File) ->
  Type0 = case mimetypes:filename(File) of
    [Type|_] -> Type;
    Type -> Type
  end,
  [Type1|Rest] = binary:split(Type0),
  {Type1, lists:foldr(fun(A, B) ->
          if
            bit_size(B) > 0 -> <<A/binary, "/", B/binary>>;
            true -> A
          end
      end, <<>>, Rest)}.

test1() ->
  {From, To, Mail} = MM = send(
      <<"gregoire.lejeune@gmail.com">>,
      [{<<"To">>, <<"gregoire.lejeune@free.fr">>},
       {<<"Cc">>, [<<"gregoire.lejeune@gmail.com">>, <<"lejeune.gregoire@me.com">>]}],
      <<"Test gen_smtp">>,
      [{text, <<"Contenu au format text">>},
       {html, <<"Contenu au format <b>html</b>">>}],
      [<<"/Users/glejeune/Dropbox/Greg/images/greg.jpg">>]),
  io:format("FROM : ~p~n", [From]),
  io:format("TO   : ~p~n", [To]),
  file:write_file("test.mel", Mail),
  io:format("~s~n", [Mail]),
  gen_smtp_client:send_blocking(MM, [
      {relay, "localhost"},
      {port, 1025}
      ]).
     
test2() ->
  {From, To, Mail} = MM = send(
      <<"Greg <gregoire.lejeune@gmail.com>">>,
      [{<<"To">>, <<"gregoire.lejeune@free.fr">>},
       {<<"Cc">>, [<<"gregoire.lejeune@gmail.com">>, <<"lejeune.gregoire@me.com">>]}],
      <<"Test gen_smtp">>,
      [{text, <<"Contenu au format text">>},
       {html, <<"Contenu au format <b>html</b>">>}],
      []),
  io:format("FROM : ~p~n", [From]),
  io:format("TO   : ~p~n", [To]),
  file:write_file("test.mel", Mail),
  io:format("~s~n", [Mail]),
  gen_smtp_client:send_blocking(MM, [
      {port, 1025},
      {relay, "localhost"}
      ]).
