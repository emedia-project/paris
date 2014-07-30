-module(paris_mailer).

-callback from() -> string() | binary().
-callback subject() -> string() | binary().
-callback cc() -> string | binary() | [string()] | [binary()].
-callback bcc() -> string | binary() | [string()] | [binary()].
-callback template() -> atom().
-callback done(Response :: any()) -> any().
