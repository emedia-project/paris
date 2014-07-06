-module(posts).
-compile([{parse_transform, texas_transform}]).

-field({id,       [{type, id},           {autoincrement, true}]}).
-field({author,   [{type, string},       {len, 512}           ]}).
-field({title,    [{type, string},       {len, 512}           ]}).
-field({body,     [{type, text}                               ]}).
-field({date,     [{type, string},       {len, 32}            ]}).
-field({comments, [{has_many, comments}                       ]}).
