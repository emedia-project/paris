-module(paris_websocket).

-type opts() :: any().
-type state() :: any().
-type terminate_reason() :: {normal, shutdown}
	| {normal, timeout}
	| {error, closed}
	| {remote, closed}
	| {remote, cowboy_websocket:close_code(), binary()}
	| {error, badencoding}
	| {error, badframe}
	| {error, atom()}.

-callback init(atom(), Req, opts())
	-> {ok, Req, state()}
	| {ok, Req, state(), hibernate}
	| {ok, Req, state(), timeout()}
	| {ok, Req, state(), timeout(), hibernate}
	| {shutdown, Req}
	when Req::cowboy_req:req().
-callback handle({text | binary | ping | pong, binary()}, Req, State)
	-> {ok, Req, State}
	| {ok, Req, State, hibernate}
	| {reply, cowboy_websocket:frame() | [cowboy_websocket:frame()], Req, State}
	| {reply, cowboy_websocket:frame() | [cowboy_websocket:frame()], Req, State, hibernate}
	| {shutdown, Req, State}
	when Req::cowboy_req:req(), State::state().
-callback info(any(), Req, State)
	-> {ok, Req, State}
	| {ok, Req, State, hibernate}
	| {reply, cowboy_websocket:frame() | [cowboy_websocket:frame()], Req, State}
	| {reply, cowboy_websocket:frame() | [cowboy_websocket:frame()], Req, State, hibernate}
	| {shutdown, Req, State}
	when Req::cowboy_req:req(), State::state().
-callback terminate(terminate_reason(), cowboy_req:req(), state())
	-> ok.
