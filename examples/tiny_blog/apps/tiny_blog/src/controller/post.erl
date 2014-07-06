-module(post).

-export([
  get/1,
  get/2,
  post/1
]).

get(_Request) ->
  paris_response:render_view(new_post).

get(_, ID) ->
  DB = paris:get(db),
  Post = posts:find(DB, first, [{where, [{id, ID}]}]),
  Comments = lists:map(fun(C) ->
          C:to_keylist()
      end, Post:comments()),
  paris_response:render_view(view_post, [{post, Post:to_keylist()}, {comments, Comments}]).

post(Request) ->
  DB = paris:get(db),
  Title = binary_to_list(paris_request:param(Request, <<"title">>)),
  Author = binary_to_list(paris_request:param(Request, <<"author">>)),
  Body = estring:gsub(
      binary_to_list(paris_request:param(Request, <<"body">>)),
      "\r\n", "<br />"),
  Date = ec_date:format("d/m/Y"),
  Post = posts:new(DB, [
      {title, Title},
      {author, Author},
      {date, Date},
      {body, Body}]),
  Post1 = Post:insert(),
  paris_response:redirect("/post/~p", [Post1:id()]).

