-module(paris_mail).

-export([
  send/4,
  deliver/3,
  deliver/4
  ]).

deliver(Module, To, Data) ->
  deliver(Module, To, Data, []).
deliver(Module, To, Data, Options) ->
  send(
    Module:from(),
    To,
    elists:keyfind(subject, 1, Options, Module:subject()),
    [{template, Module:template(), Data},
     {cc, elists:keyfind(cc, 1, Options, []) ++ Module:cc()},
     {bcc, elists:keyfind(bcc, 1, Options, []) ++ Module:bcc()},
     {callback, fun Module:done/1}]).

%% @doc
%% Send an email
%%
%% Options:
%% <pre>
%% {cc, string() | binary() | [string()] | [binary()]}
%% {bcc, string() | binary() | [string()] | [binary()]}
%% {template, atom(), [{atom(), any()}]}
%% {body, string() | binary()}
%% {attachment, [string()] | [binary()]}
%% {callback, function()}
%% </pre>
%%
%% Example:
%% <pre>
%% send(
%%   "greg@example.com", 
%%   ["bob@example.com", "john@example.com"]
%%   "This is a mail",
%%   [{cc, ["tania@example.com", "tom@example.com"]},
%%    {bcc, "jane@example.com"},
%%    {template, my_template, Data},
%%    {attachments, ["/home/greg/photo.png"]}
%%    {callback, Fun module:function/1}]).
%% </pre>
%% @end
-spec send(string() | binary(), string() | binary() | [string()] | [binary()], string() | binary(), list()) -> {ok, pid()} | {error, any()}.
send(From, To, Subject, Options) ->
  BFrom = eutils:to_binary(From),
  BSubject = eutils:to_binary(Subject),
  Dest = [{<<"To">>, to_list_of_binary(To)}] ++
         case lists:keyfind(cc, 1, Options) of
    {cc, Data0} -> [{<<"Cc">>, to_list_of_binary(Data0)}];
    _ -> []
  end ++ case lists:keyfind(bcc, 1, Options) of
    {bcc, Data1} -> [{<<"Bcc">>, to_list_of_binary(Data1)}];
    _ -> []
  end,
  Attachments = case lists:keyfind(attachments, 1, Options) of
    {attachments, Data2} -> to_list_of_binary(Data2);
    false -> []
  end,
  Body = case lists:keyfind(template, 1, Options) of
    {template, Template, TemplateData} -> 
      HtmlTemplate = list_to_atom(atom_to_list(Template) ++ "_html"),
      TextTemplate = list_to_atom(atom_to_list(Template) ++ "_txt"),
      case eutils:module_exist(HtmlTemplate) of
        false -> [];
        true -> 
          {ok, Html} = HtmlTemplate:render(TemplateData), 
          [{html, ebinary:concat(Html)}]
      end ++ case eutils:module_exist(TextTemplate) of
        false -> [];
        true -> 
          {ok, Text} = TextTemplate:render(TemplateData), 
          [{html, ebinary:concat(Text)}]
      end;
    _ -> case lists:keyfind(body, 1, Options) of
        {body, Data3} -> [{text, eutils:to_binary(Data3)}];
        _ -> [{text, <<"">>}]
      end
    end,
  Callback = elists:keyfind(callback, 1, Options, undefined),
  do_send(BFrom, Dest, BSubject, Body, Attachments, Callback).

to_list_of_binary(Data) ->
  case eutils:is_string(Data) of
    true -> [list_to_binary(Data)];
    false -> case is_binary(Data) of
        true -> [Data];
        false -> lists:map(fun eutils:to_binary/1, Data)
      end
  end.

% do_send(
%   <<"gregoire.lejeune@gmail.com">>,
%   [{<<"To">, <<"gregoire.lejeune@free.fr">>},
%    {<<"Cc">, [<<"gregoire.lejeune@gmail.com">>, <<"lejeune.gregoire@me.com">>]}],
%   <<"Test gen_smtp">>,
%   [{text, <<"Contenu au format text">>},
%    {html, <<"Contenu au format <b>html</b>">>}],
%   [<<"/Users/glejeune/Dropbox/Greg/images/greg.jpg">>],
%   Callback).
do_send(From, Dest, Subject, Bodies, Attachments, Callback) ->
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
  gen_smtp_client:send(
    {binary_to_list(From), dests_list(Dest), Mail},
    paris:mailconf(),
    Callback).
  
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

