-module(comment).

-export([
  post/2
]).

post(Request, ID) ->
  DB = paris:get(db),
  Post = posts:find(DB, first, [{where, [{id, ID}]}]),
  Author = binary_to_list(paris_request:param(Request, <<"author">>)),
  Body = estring:gsub(
      binary_to_list(paris_request:param(Request, <<"body">>)),
      "\r\n", "<br />"),
  Date = ec_date:format("d/m/Y"),
  Comment = comments:new(DB, [
      {author, Author},
      {date, Date},
      {body, Body},
      {post, Post}]),
  _Comment1 = Comment:insert(),
  paris_response:redirect("/post/~s",[ID]).

