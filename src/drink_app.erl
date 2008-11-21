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
	yaws_conf(),
	drink_sup:start_link(StartArgs).

stop(_State) ->
	ok.

yaws_conf() ->    
	{ok, WebLog} = application:get_env(drink, weblogs),
	{ok, Docroot} = application:get_env(drink, docroot),
	GC0 = yaws_config:make_default_gconf(false, "drinkweb"),
	GC = GC0#gconf{
	    logdir = WebLog,
	    flags = yaws:flag(GC0#gconf.flags, ?GC_COPY_ERRLOG, false)
	},
	SC = #sconf{
	    port = 80,
	    servername = "drink.csh.rit.edu",
	    listen = {0,0,0,0},
	    docroot = "",
	    allowed_scripts = [],
	    appmods = [{"/", ssl_redirect}]},
	Ssl = #ssl{
	    keyfile = filename:join(code:priv_dir(drink), "key.pem"),
	    certfile = filename:join(code:priv_dir(drink), "cert.pem"),
	    cacertfile = filename:join(code:priv_dir(drink), "cshca.crt")},
	SCssl = #sconf{
	    port = 443,
	    servername = "drink.csh.rit.edu",
	    listen = {0,0,0,0},
	    docroot = Docroot,
	    allowed_scripts = [yaws],
	    ssl = Ssl,
	    appmods = [{"/drink", drink_web}]},
	yaws_api:setconf(GC, [[SC], [SCssl]]).