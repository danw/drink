-module (drink_web).
-behaviour (gen_server).

-export ([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

init({}) ->
	io:format("Starting web server...~n"),
	Res = mochiweb_http:start([{port, 80}, {name, drink_web_server}, {loop, fun handle_message/1}]),
	io:format("Got ~w~n", [Res]),
	{ok, {}}.

handle_call(_, _, _) ->
	{reply, {error}, {}}.
handle_cast(_, _) ->
	{noreply, {}}.
handle_info(_, _) ->
	{noreply, {}}.

terminate(_Reason, _State) ->
	mochiweb_http:stop(drink_web_server),
	ok.
code_change(_, _, _) ->
	{ok, {}}.

handle_message(Req) ->
	Path = Req:get(path),
	Method = Req:get(method),
	Post = Req:parse_post(),
	handle(Req, Method, Path, Post).

handle(Req, 'GET', "/", _) ->
	Req:respond({200, [{"Content-Type", "text/html"}], "Testing Reload"});
handle(Req, 'GET', _, _) ->
	Req:not_found().