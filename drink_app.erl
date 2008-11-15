-module (drink_app).
-behaviour (application).

-export ([start/2, stop/1]).

start(_Type, StartArgs) ->
	case mnesia:start() of
		{error, _Reason} ->
			drink_mnesia:initialize();
		ok ->
			drink_mnesia:upgrade()
	end,
	drink_sup:start_link(StartArgs).

stop(_State) ->
	ok.