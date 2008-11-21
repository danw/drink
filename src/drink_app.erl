-module (drink_app).
-behaviour (application).

-export ([start/2, stop/1]).

-include ("yaws.hrl").

start(_Type, StartArgs) ->
	case mnesia:start() of
		{error, _Reason} ->
			drink_mnesia:initialize();
		ok ->
			drink_mnesia:upgrade()
	end,
	GC = yaws_config:make_default_gconf(false),
	SC = #sconf{
	    port = 80,
	    servername = "drinkweb",
	    listen = {0,0,0,0},
	    docroot = application:get_env(drink, docroot)},
	yaws_api:setconf(GC, [[SC]]),
	drink_sup:start_link(StartArgs).

stop(_State) ->
	ok.